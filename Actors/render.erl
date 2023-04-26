%Render actors rapresent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
% Compare this snippet from Actors\ambient.erl:

-module(render).
-export([main/0,order_chessboard/1]).

print([{{X,Y}, STATUS} | Tail],N)-> 
    case X == N of
        true -> 
            case STATUS == undefined of
                true -> 
                    io:fwrite("O  ");
                    false -> 
                        io:fwrite("X  ")
            end,
            print(Tail,N);
        false -> 
            io:format("~n"),
            print(Tail,N+1)
    end;

print([],_)-> ok.



order_chessboard(Chessboard) ->
    lists:sort(fun({{X1,Y1},_}, {{X2,Y2},_}) ->
                                case X1 < X2 of
                                    true -> true;
                                    false -> 
                                        case X1 == X2 of
                                            true -> Y1 < Y2;
                                            false -> false
                                        end
                                end
                            end,
                            dict:to_list(Chessboard)).

main() ->
    receive
       {Chess} -> 
            %io:fwrite("Chessboard:~n"),
            L = order_chessboard(Chess),
            %io:format("~p ~n",[L]),	
            %io:format("Chessboard ~p~n", [list:sort(list:to_list(Chess))]),
            %print([L],1),
            print(L,1)
            %io:fwrite("DIOCANE~n")
            %main()
    end. 
