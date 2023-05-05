%Render actors represent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
-module(render).
-export([main/2, order_chessboard/1]).

print_chessboard([{X,Y}|T], Dict) ->
    case Y == 1 of
        true -> io:format("\n");
        _ -> ok
    end,
    A = lists:keyfind({X,Y}, 2, Dict),
    case A of
        false -> io:format("O\t");
        {_,{X,Y}} -> io:format("X\t")
    end,
    %io:format("A IS EQUAL TO ~p~n", [Dict]),
    print_chessboard(T,Dict );


print_chessboard([],_)-> ok.


%Function to print a list 
print_list([H|T]) -> 
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
    receive
        {position, PID, X, Y} -> 
            timer:sleep(3000),
            Dict2 = dict:store(PID, {X,Y}, Dict),
            List = order_chessboard(dict:to_list(Dict2)),
            print_chessboard(Chessboard, List),
            main(Chessboard, Dict2)
    end. 
