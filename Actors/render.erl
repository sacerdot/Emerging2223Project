%Render actors represent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
-module(render).
-export([main/2, order_chessboard/1]).

print_chessboard([{X,Y}|T], DictToList) ->
    case Y == 0 of
        true -> io:format("\n");
        _ -> ok
    end,
    A = lists:keyfind({X,Y}, 2, DictToList),
    B = lists:keyfind({X,Y}, 3, DictToList),
    case {A,B} of 
        {false, false} -> io:format("O\t");
        {{_,{X,Y},_}, {_,_,{X,Y}}} -> io:format("X*\t");
        {{_,{X,Y},_}, _} -> io:format("X\t");
        {_ , {_,_,{X,Y}}} -> io:format("*\t")
    end,
   
    %io:format("A IS EQUAL TO ~p~n", [Dict]),
    print_chessboard(T, DictToList );


print_chessboard([],_)-> io:format("\n").


%Function to print a list 
print_list([H|T]) -> 
    %map element of list {A, {B,C}} to {A, B, C}
    io:fwrite("~p~n", [H]),
    print_list(T);
print_list([]) -> ok.


order_chessboard(Chessboard) ->
    lists:sort(fun({_, {X1,Y1}}, { _, {X2,Y2}}) ->
                                case X1 < X2 of
                                    true -> true;
                                    false -> 
                                        case X1 == X2 of
                                            true -> Y1 < Y2;
                                            false -> false
                                        end
                                end
                            end,
                            Chessboard).

main(Chessboard, Dict) ->
    % PID -> {{POS},{GOAL}}     DICT STRUCTURE
    receive
        % position of car sent by detect
        {position, PID, X, Y} -> 
            case dict:find(PID, Dict) of 
                error -> 
                    Dict2 = dict:store(PID, {{X,Y}, {undefined, undefined}}, Dict),
                    Dict2;
                {ok, {{_,_},{X_Goal, Y_Goal}}} ->
                     Dict2 = dict:store(PID, {{X,Y},{X_Goal, Y_Goal}}, Dict),
                     Dict2
            end,
            DictToList = dict:to_list(Dict2),
            DictToList2 = lists:map(fun({A, {B,C}}) -> {A,B,C} end, DictToList),
            print_list(DictToList2),
            print_chessboard(Chessboard, DictToList2),
            main(Chessboard, Dict2);
        % target position of goal sent by detect
        {target, PID, X, Y} ->    
            case dict:find(PID, Dict) of 
                error -> 
                    Dict2 = dict:store(PID, {{undefined, undefined},{X,Y}}, Dict),
                    Dict2;
                {ok, {{X_Pos, Y_Pos}, {_,_}}} ->
                     Dict2 = dict:store(PID, {{X_Pos, Y_Pos},{X,Y}}, Dict),
                     Dict2
            end,
            DictToList = dict:to_list(Dict2),
            DictToList2 = lists:map(fun({A, {B,C}}) -> {A,B,C} end, DictToList),
            print_list(DictToList2),
            print_chessboard(Chessboard, DictToList2),
            main(Chessboard, Dict2)
    end. 
