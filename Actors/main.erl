-module(main).
-export([main/0]).

main() ->
    H = 5,
    W = 5,
    Chessboard = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)]),
	
    PID_R = spawn(render, main, [[{X, Y} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)], dict:new()]),
    register(render, PID_R),
    io:format("MAIN: Correctly registered ~p as 'render' ~n", [PID_R]), %DEBUG
    render ! {dict:to_list(Chessboard)}, %DEBUG
    
    PID_A = spawn(ambient, ambient, [Chessboard]), %spawn the ambient actor
    register(ambient, PID_A), %register the ambient actor with the name ambient
    io:format("MAIN: Correctly registered ~p as 'ambient' ~n", [PID_A]), %DEBUG

    PID_W = spawn(wellknown, wellknown, [[]]),
    register(wellknown, PID_W),
    io:format("MAIN: Wellknown started with PID ~p registered with wellknown~n", [PID_W]),

    Spawn_loop = fun Spawn_loop(N, List) ->
        case N > 0 of
            true -> 
                {PID_M, Ref_monitor} = spawn_monitor(car, main, [H,W]),  
                io:format("WN: Spawned car with PID: ~p~n", [PID_M]),
                List2 = lists:append(List, [PID_M]),
                timer:sleep(5000),
                Spawn_loop(N-1, List2);
            false ->  
                    %choose the first car from the list, kill and remove it 
                    case length(List) of 
                        12 ->
                            PID_M2 = lists:nth(7, List),
                            exit(PID_M2, kill),
                            List3 = [ {PIDF, PIDS} || {PIDF, PIDS} <- List, PIDF =/= PID_M2],
                            Spawn_loop(0, List3);
                        _-> ok
                    end,
                    receive
                        {'DOWN', _, _, PID, Reason } ->
                            io:format("WN: Died PID: ~p, Reason: ~p~n", [PID, Reason]),
                            Spawn_loop(0, List)
                        end %end of receive
        end %end of case
    end, % end of fun
    Spawn_loop(12, []).
