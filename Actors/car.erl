-module(car).
    -export([main_car/2]).

    %The main actor creates other actors and re-creates them if they fail
    main_car(H, W) ->
        {X_Spawn, Y_Spawn} = generate_coordinates(H, W),
        {X_Goal, Y_Goal} = generate_coordinates(H, W),
        %PID_S receive a dict with all the free parking
        PID_S = spawn(?MODULE, state, [dict:from_list([{{X, Y}, true} || X <- lists:seq(1, H), Y <- lists:seq(1, W)]), X_Goal, Y_Goal, H, W]),
        PID_D = spawn(?MODULE, detect, [X_Spawn, Y_Spawn, X_Goal, Y_Goal, H, W, PID_S]),
        PID_F = spawn(?MODULE, friendship, []).
        %TODO: handling respawn

    friendship() ->
        io:fwrite("CIAO\n").

    state(World_Knowledge, X_Goal, Y_Goal, H, W) ->
        receive
            {updateState, PID_D,  X, Y, isFree} -> 
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
                end,
                state(dict:store({X,Y}, isFree, World_Knowledge), X_Goal, Y_Goal, H, W)
        end,
        state(World_Knowledge, X_Goal, Y_Goal, H, W).

    generate_coordinates(H, W) ->
        X = random:uniform(H),
        Y = random:uniform(W),
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
        case X =:= X_Goal andalso Y =:= Y_Goal of
            true -> 
                Park_ref = make_ref(),
                ambient ! {park, self(), X, Y, Park_ref}, %TODO: we must ask isFree before parking? 
                timer:sleep(random:uniform(5)*1000), 
                ambient ! {leave, self(), Park_ref},
                %TODO: ask new goal
                {X, Y};
            false ->
                case X =:= X_Goal of
                    true -> {X, (Y + compute_Y_movement(Y, Y_Goal, W)) rem W};
                    false ->
                        case Y =:= Y_Goal of
                            true -> {(X + compute_X_movement(X, X_Goal, H)) rem H, Y};
                            false ->
                                case random:uniform(2) of
                                    1 -> {(X + compute_X_movement(X, X_Goal, H)) rem H, Y};
                                    2 -> {X, (Y + compute_Y_movement(Y, Y_Goal, W)) rem W}
                                end
                        end
                end
        end.
    
    %%%%%%
    %@param X:  actual X coordinate of the car
    %@param Y:  actual Y coordinate of the car
    %
    detect(X, Y, X_Goal, Y_Goal, H, W, PID_S) ->
        {X_New, Y_New} = move(X, Y, {X_Goal, Y_Goal}, H, W), %TODO: H and W must be passed as parameters? 
        ambient ! {isFree, self(), X_New, Y_New, make_ref()},
        receive 
            {status, Ref, isFree} -> 
                PID_S ! {updateState, self(), X_New, Y_New, isFree}
            %TODO: If i'm in goal wait the status of the goal from the ambient else 
            %{updateGoal, X_Goal_New, Y_Goal_New} -> 
            %detect(X_New, Y_New, X_Goal_New, Y_Goal_New, H, W, PID_S);
        end, 
        timer:sleep(2000),
        detect(X_New, Y_New, X_Goal, Y_Goal, H, W, PID_S).

       


    

