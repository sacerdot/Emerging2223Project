%Render actors represent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
-module(render).
-export([main/2, render/3]).

print_chessboard([{X,Y}|T], DictToList) ->
    case Y == 0 of
        true -> io:format("\n");
        _ -> ok
    end,
    A = lists:keyfind({X,Y}, 2, DictToList),
    B = lists:keyfind({X,Y}, 3, DictToList),
    %The struct is {PID, {POS}, {GOAL}, [FRIENDS], ID}
    case {A,B} of 
        {false, false} -> io:format("O\t");
        {{_,{X,Y},_,_, ID}, {_,_,{X,Y}, _, ID}} -> io:format("*~p*\t", [ID]);
        {{_,{X,Y},_,_, ID}, _} -> io:format("~p\t", [ID]);
        {_ , {_,_,{X,Y},_, ID}} -> io:format("~p*\t", [ID])
    end,
   
    %io:format("A IS EQUAL TO ~p~n", [Dict]),
    print_chessboard(T, DictToList );
    print_chessboard([],_)-> io:format("\n").


transform_print(Chessboard, NewDict) ->
    DictToList = dict:to_list(NewDict),
    DictToList2 = lists:map(fun({A, {B,C,D,E}}) -> io:format("R: Car ~p with PID ~p is Friend with ~p~n", [E,A,D]), {A,B,C,D,E} end, DictToList),
    print_chessboard(Chessboard, DictToList2).


render(Chessboard, Dict, N) ->
    % PID -> {{POS},{GOAL}, int}     DICT STRUCTURE
    receive
        {print} ->
            transform_print(Chessboard, Dict),
            render(Chessboard, Dict, N);
        % position of car sent by detect
        {position, PID, X, Y} -> 
            case dict:find(PID, Dict) of 
                error -> 
                    Dict2 = dict:store(PID, {{X,Y}, {undefined, undefined}, [], N}, Dict),
                    monitor(process, PID),
                    %transform_print(Chessboard, Dict2),
                    render(Chessboard, Dict2, N+1);
                {ok, {{_,_},{X_Goal, Y_Goal}, FriendsList, ID}} ->
                    Dict2 = dict:store(PID, {{X,Y},{X_Goal, Y_Goal}, FriendsList, ID}, Dict),
                    %transform_print(Chessboard, Dict2),
                    render(Chessboard, Dict2, N)
            end;
            
        % target position of goal sent by detect
        {target, PID, X, Y} ->    
            case dict:find(PID, Dict) of 
                error -> 
                    Dict2 = dict:store(PID, {{undefined, undefined},{X,Y}, [], N}, Dict),
                    monitor(process, PID),
                    %transform_print(Chessboard, Dict2),
                    render(Chessboard, Dict2, N+1);
                {ok, {{X_Pos, Y_Pos}, {_,_}, FriendsList, ID}} ->
                    Dict2 = dict:store(PID, {{X_Pos, Y_Pos},{X,Y}, FriendsList, ID}, Dict),
                    %transform_print(Chessboard, Dict2),
                    render(Chessboard, Dict2, N)
            end;
        %sent by ambient when car park or restart
        {parked, PID, X, Y, IsParked} -> 
            {_,_,_, Num} = dict:fetch(PID, Dict),
            io:format("RENDER: Car N_~p with PID ~p is parked at (~p, ~p): ~p~n", [Num, PID, X, Y, IsParked]),
            render(Chessboard, Dict, N);
        %sent by friendship actor TODO: implement this 
        {friends, PID, PIDLIST} -> 
            case dict:find(PID, Dict) of 
                error -> 
                    Dict2 = dict:store(PID, {{undefined, undefined}, {undefined, undefined}, PIDLIST, N}, Dict),
                    monitor(process, PID),
                    %transform_print(Chessboard, Dict2),
                    render(Chessboard, Dict2, N+1);

                {ok, {{X_Pos, Y_Pos}, {X_Goal, Y_Goal}, _, ID}} ->
                    Dict2 = dict:store(PID, {{X_Pos, Y_Pos},{X_Goal,Y_Goal}, PIDLIST ,ID}, Dict),
                    %transform_print(Chessboard, Dict2),
                    %io:format("RENDER: Car N_~p with PID: ~p is friend with ~p~n", [ID, PID, PIDLIST]),
                    render(Chessboard, Dict2, N)
            end;
        {'DOWN', _, _, PID, Reason } ->
                    {_,_,_, Num} = dict:fetch(PID, Dict),
                    io:format("RENDER: Died PID: ~p with N_~p, Reason: ~p~n", [PID, Num, Reason]),
                    %remove from dict
                    Dict2 = dict:erase(PID, Dict),
                    render(Chessboard, Dict2, N)
    end. 

main(Chessboard, Dict) ->
    PID_R = spawn(render, render, [Chessboard, Dict, 1]),
    register(render, PID_R),
    io:format("RENDER: Correctly registered ~p as 'render' ~n", [PID_R]),
    PID_R. %DEBUG
