-module(car).
    -export([main_car/2, friendship/3, state/5, detect/7, getFriends/2]).



    friendship(FriendsList, RefList, PID_S) when lenght(FriendsList) < 5 ->
        L = length(FriendsList),
        Ref = make_ref(),
        case L of 
            0 -> wellknown ! {getFriends, self(), PID_S ,Ref};
            _ ->   getFriends(FriendList, RefList)
        end
        receive
           {myFriends, PIDSLIST, Ref} ->
                %Demonitor the old friends from refList
                lists:foreach(fun(X) -> demonitor(X) end, RefList),
                TotalFriends = PIDSLIST ++ FriendsList,
                %Create a list choosing 5 random friends from TotalFriends
                FriendList2 = lists:sublist([Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- TotalFriends])], 5),
                %Monitor all the friends in the list and save the ref in RefList
                RefList2 = lists:map(fun({PIDF, _}) -> monitor(process, PIDF) end, FriendList2),
                friendship(FriendList2, RefList2, PID_S)
        end.

    getFriends(FriendsList, RefList) ->
        receive
           {myFriends, PIDSLIST, Ref} ->
                %Demonitor the old friends from refList
                lists:foreach(fun(X) -> demonitor(X) end, RefList),
                TotalFriends = PIDSLIST ++ FriendsList,
                %Create a list choosing 5 random friends from TotalFriends
                FriendList2 = lists:sublist([Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- TotalFriends])], 5),
                %Monitor all the friends in the list and save the ref in RefList
                RefList2 = lists:map(fun({PIDF, _}) -> monitor(process, PIDF) end, FriendList2),
        end.
                

    state(World_Knowledge, X_Goal, Y_Goal, H, W) ->
        io:format("Start State~n"),
        receive
            {updateState, PID_D, X, Y, isFree} -> 
                io:fwrite("Update Value for park (~p,~p), new value: ~p~n", [{X,Y}, isFree]), %Update parkings  
                %TODO: send the new info to friends
                case isFree of
                    %If a park becames free, the state actor updates the knowledge of the world
                    %It doesn't update the goal coordinates because this case doesn't affect s
                    true -> 
                        state(dict:store({X,Y}, isFree, World_Knowledge), X_Goal, Y_Goal, H, W);
                    %If a park becames busy I have to check if it was the goal
                    false -> 
                        case {X=:=X_Goal, Y =:= Y_Goal} of
                            %If the goal is busy, I have to generate a new goal
                            {true,true} -> 
                                io:fwrite("New Goal: ~p~n", [{X,Y}]),
                                {X_Goal_New, Y_Goal_New} = generate_coordinates(H, W), 
                                PID_D ! {updateGoal, X_Goal_New, Y_Goal_New},
                                state(dict:store({X,Y}, isFree, World_Knowledge), X_Goal_New, Y_Goal_New, H, W);
                            %Else update the knowledge of the world
                            {_,_} -> 
                                state(dict:store({X,Y}, isFree, World_Knowledge), X_Goal, Y_Goal, H, W)
                        end
                end;
            %Case Car Exit from the parking and needs new goal
            {askNewGoal, PID_D, Ref} -> 
                io:format("Ask New Goal with Ref ~p~n",[Ref]),
                {X_Goal_New, Y_Goal_New} = generate_coordinates(H, W), %TODO: check if new coordinates I generate should be free?
                PID_D ! {responseNewGoal, X_Goal_New, Y_Goal_New, Ref},
                state(World_Knowledge, X_Goal_New, Y_Goal_New, H, W);

            _ -> state( World_Knowledge, X_Goal, Y_Goal, H, W)
        end.

    generate_coordinates(H, W) ->
        X = rand:uniform(H)-1,
        Y = rand:uniform(W)-1,
        {X, Y}.

%L'attore "detect" di un'automobile sceglie un posteggio obiettivo libero interagendo con l'attore "state". Dopodichè, ogni 2s, si avvicina di una cella verso tale obiettivo. Se deve muoversi lungo entrambi gli assi (x e y), lo fa scegliendo randomicamente l'asse e muovendosi nella direzione che minimizza la distanza percorsa.
    
    %%%%
    % @param X:  actual X coordinate of the car
    % @param X_Goal: X coordinate of the goal
    % @param H: height of the chessboard
    % @return: 1 or -1
    % @dev: This function computes the movement along the X axis that minimizes the distance to the goal.
    %       If the distance is the same, the function returns 1. Since pacman effect is allowed (the car can move from the last cell to the  
    %       first one and viceversa), the function must consider the modulo operation.
    compute_X_movement(X, X_Goal, H) ->
        D_pos = abs(X_Goal - ((X+1) rem H)), 
        D_neg = abs(X_Goal - ((X-1) rem H)),
        %io:format("X: D_pos: ~p, D_neg: ~p~n", [D_pos, D_neg]),
        case D_pos =< D_neg of
            true -> 1;
            false -> -1
        end.

    %%%%
    % @param Y:  actual Y coordinate of the car
    % @param Y_Goal: Y coordinate of the goal
    % @param W: width of the chessboard
    % @return: 1 or -1
    % @dev: This function computes the movement along the Y axis that minimizes the distance to the goal.
    %       If the distance is the same, the function returns 1. Since pacman effect is allowed (the car can move from the last cell to the
    %       first one and viceversa), the function must consider the modulo operation.
    compute_Y_movement(Y, Y_Goal, W) ->
        D_pos = abs(Y_Goal - ((Y+1) rem W)), 
        D_neg = abs(Y_Goal - ((Y-1) rem W)),
        %io:format("Y: D_pos: ~p, D_neg: ~p~n", [D_pos, D_neg]),
        case D_pos =< D_neg of
            true -> 1;
            false -> -1
        end.

    %%%%
    % @param X:  actual X coordinate of the car
    % @param Y:  actual Y coordinate of the car
    % @param X_Goal: X coordinate of the goal
    % @param Y_Goal: Y coordinate of the goal
    % @param H: height of the chessboard
    % @param W: width of the chessboard
    % @return: {X,Y} coordinates of the next cell
    % @dev: This function computes the next cell to reach the goal. 
    %       If the car is already in the goal this function sends the message "park" to the ambient actor.
    %       If the car is not in the goal, the function computes the next cell to reach the goal. 
    %       If the car is not in the goal and it must move along both the axes, the function chooses randomly the axis and the direction.
    move(X, Y, {X_Goal, Y_Goal}, H, W) ->

        case {X =:= X_Goal, Y =:= Y_Goal} of
            {true, true} -> 
                {X, Y};
            {true, false}-> 
                {X, (Y + compute_Y_movement(Y, Y_Goal, W)) rem (W)};
            {false, true} -> 
                {(X + compute_X_movement(X, X_Goal, H)) rem (H), Y};
            {false, false} ->
                case random:uniform(2) of
                    1 -> {(X + compute_X_movement(X, X_Goal, H+1)) rem (H), Y};
                    2 -> {X, (Y + compute_Y_movement(Y, Y_Goal, W+1)) rem (W)}
                end
        end. 
    %%%%%%
    %@param X:  actual X coordinate of the car
    %@param Y:  actual Y coordinate of the car
    %
    detect(X, Y, X_Goal, Y_Goal, H, W, PID_S) ->
        io:format("Start Detect with goal (~p,~p)~n", [X_Goal, Y_Goal]),
        link(PID_S),
        timer:sleep(2000),
        {X_New, Y_New} = move(X, Y, {X_Goal, Y_Goal}, H, W), %TODO: H and W must be passed as parameters? 
        render ! {position, self(), X_New, Y_New},
        timer:sleep(5000), %TODO: just for debug()
        Ref = make_ref(),
        %io:format("Ref ~p~n", [Ref]),
        ambient ! {isFree, self(), X_New, Y_New, Ref},
        %io:format("I'm here ~p~n",[self()]),
        receive 
            {updateGoal, X_Goal_New, Y_Goal_New} ->  detect(X_New, Y_New, X_Goal_New, Y_Goal_New, H, W, PID_S);
            {status, Ref, IsFree} -> 
                %io:format("Received status ~p with Ref ~p~n", [IsFree, Ref]),
                PID_S ! {updateState, self(), X_New, Y_New, IsFree},
                case {X_New =:= X_Goal, Y_New =:= Y_Goal} of
                    {true, true} ->
                        Park_Ref = make_ref(),
                        ambient ! {park, self(), X_New, Y_New, Park_Ref},
                        timer:sleep(rand:uniform(5)*1000),
                        ambient ! {leave, self(), Park_Ref},
                        PID_S ! {askNewGoal, self(), Park_Ref},
                        receive
                            {responseNewGoal, X_Goal_New, Y_Goal_New, Park_Ref} ->
                                io:format("Received new goal (~p, ~p) with Ref: ~p~n", [X_Goal_New, Y_Goal_New, Park_Ref]),
                                render ! {target, self(), X_Goal_New, Y_Goal_New},
                                detect(X_New, Y_New, X_Goal_New, Y_Goal_New, H, W, PID_S);
                            Msg -> io:fwrite("MSG: ~p~n",[Msg]) %TODO: Kill process?
                        end;
                    _ -> detect(X_New, Y_New, X_Goal, Y_Goal, H, W, PID_S)
                
                end; 
            _ -> io:fwrite("No Pattern Matching Found~n") %TODO: Kill process?
        end.

    %The main actor creates other actors and re-creates them if they fail
    main_car(H, W) ->
        process_flag(trap_exit, true), 
        {X_Spawn, Y_Spawn} = generate_coordinates(H, W),
        {X_Goal, Y_Goal} = generate_coordinates(H, W),
        io:format("X_Spawn: ~p, Y_Spawn: ~p~n", [X_Spawn, Y_Spawn]),
        io:format("X_Goal: ~p, Y_Goal: ~p~n", [X_Goal, Y_Goal]),

        Spawn_loop = fun Spawn_loop() ->
            PID_S = spawn(?MODULE, state, [dict:new(), X_Goal, Y_Goal, H, W]),
            {PID_D, Ref_monitor} = spawn_monitor(?MODULE, detect, [X_Spawn, Y_Spawn, X_Goal, Y_Goal, H, W, PID_S]),  
            render ! {target, PID_D, X_Goal, Y_Goal},
            render ! {position, PID_D, X_Spawn, Y_Spawn},
            receive
                {'DOWN', _, _, PID, Reason } ->
                    io:format("Died PID: ~p, Reason: ~p~n", [PID, Reason]),
                    Spawn_loop();
                X -> io:format("X: ~p~n", [X])
                 
            end
        end,
        Spawn_loop().
     

       


    

