-module(main).
-export([main/0]).

main() ->
    H = 10,
    W = 10,
    Chessboard = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)]),
	
    
    PID_A = ambient:main(Chessboard), %spawn the ambient actor
    PID_W = wellknown:main(),
    PID_R = render:main([{X, Y} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)], dict:new()),
    
    Spawn_loop = fun Spawn_loop(N, List) ->
        case N > 0 of
            true -> 
                {PID_M, Ref_monitor} = spawn_monitor(car, main, [H,W]),  
                io:format("MAIN: Spawned car with PID: ~p~n", [PID_M]),
                List2 = lists:append(List, [PID_M]),
                timer:sleep(5000),
                Spawn_loop(N-1, List2);
            false ->  
                List
        end
    end,
    List = Spawn_loop(6, []),

    Kill_spawn_loop = fun Kill_spawn_loop(CarList) ->
        %choose the first car from the list, kill and remove it 
        %N_Cars = rand:uniform(4),
        N_Cars = 1,

        Kill_Loop = fun Kill_Loop(N_Cars_to_Kill, CarList2) ->
            case N_Cars_to_Kill > 0 of
                true ->
                    %Choose randomly a car to kill
                    PID_M2 = lists:nth(rand:uniform(length(CarList2)), CarList2),
                    exit(PID_M2, kill),
                    io:format("MAIN: Killed car with PID: ~p~n", [PID_M2]),
                    CarList3 = [ {PIDF, PIDS} || {PIDF, PIDS} <- CarList2, PIDF =/= PID_M2],
                    Kill_Loop(N_Cars_to_Kill-1, CarList3);
                false ->
                    CarList2
            end
        end,

        NewList =  Kill_Loop(N_Cars, CarList),
        Spawn_Loop = fun Spawn_Loop(N_Cars_to_Spawn, CarList2) ->
            case N_Cars_to_Spawn > 0 of
                true ->
                    {PID_M, Ref_monitor} = spawn_monitor(car, main, [H,W]),  
                    io:format("MAIN: Spawned car with PID: ~p~n", [PID_M]),
                    CarList3 = lists:append(CarList2, [PID_M]),
                    timer:sleep(5000),
                    Spawn_loop(N_Cars_to_Spawn-1, CarList3);
                false ->
                    CarList2
            end
        end,
        NewList2 = Spawn_Loop(N_Cars, NewList),
        timer:sleep(10000),
        Kill_spawn_loop(NewList2)
    end,
    Kill_spawn_loop(List).
    
                
                
                
                
    