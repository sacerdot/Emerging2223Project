%Render actors represent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
-module(render).
-export([main/0, order_chessboard/1]).

print_chessboard([{{X,Y}, STATUS} | Tail],N)-> 
    case X == N of
        true -> 
            case STATUS == undefined of
                true -> 
                    io:format("O\t");
                false -> 
                    io:format("X\t")
            end,
            print_chessboard(Tail,N);
        false -> 
            io:format("~n"),
            print_chessboard([{{X,Y}, STATUS} | Tail],N+1)
    end;

print_chessboard([],_)-> ok.


%Function to print a list 
print_list([H|T]) -> 
    io:fwrite("~p~n", [H]),
    print_list(T);
print_list([]) -> ok.


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
                            Chessboard).

main() ->
    receive
       {Chess} ->
            timer:sleep(3000), %Just to wait the print of the Chess Parameter TODO: find a better solution to do thiss
            L = order_chessboard(Chess),
            print_chessboard(L,1),
            main()
    end. 
