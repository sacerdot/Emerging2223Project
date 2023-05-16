%
-module(wellknown).
-export([main/0, wellknown/1]).


wellknown(PIDSLIST) ->
    receive
        %PID1 is the PID of friendship actor, PID2 is the PID of state actor
        {getFriends, PID1, PID2, Ref} ->
            io:format("WK: getFriends received from PID: ~p~n", [PID1]),
            PIDSLIST2 = [ {PIDF, PIDS} || {PIDF, PIDS} <- PIDSLIST, PIDF =/= PID1],
            PID1 ! {myFriends, PIDSLIST2, Ref},
            %io:format("WK: Sent myFriends to PID: ~p, myFriends:~p~n", [PID1, PIDSLIST]),
            case lists:member({PID1, PID2}, PIDSLIST) of 
                true ->  wellknown(PIDSLIST);
                false ->
                    %Monitor the friendship actor of the Car I added to my list so I can remove it if he dies
                    monitor(process, PID1), 
                    %Add the new actor to the list
                    wellknown([{PID1, PID2}|PIDSLIST])
            end;
        %case a PID I monitor dies
        {'DOWN', _, _, PID_Friendship, _Reason} ->
           %Create a new list that contains all elements form PIDSLIST except the one that died
           UpdatedList = [ {PIDF, PIDS} || {PIDF, PIDS} <- PIDSLIST, PIDF =/= PID_Friendship],
           io:format("WK: Delete PID: ~p for reason: ~p~n", [PID_Friendship, _Reason]),
           wellknown(UpdatedList)
    end.

main() ->
    PID_W = spawn(?MODULE, wellknown, [[]]),
    register(wellknown, PID_W),
    io:format("WN: Wellknown started with PID ~p registered with wellknown~n", [PID_W]),
    Spawn_loop = fun Spawn_loop(N, List) ->
            case N > 0 of
                true -> 
                    {PID_F, Ref_monitor} = spawn_monitor(car, friendship, [[],[],10]),  
                    io:format("WN: Spawned car with PID: ~p~n", [PID_F]),
                    List2 = lists:append(List, [PID_F]),
                    timer:sleep(5000),
                    Spawn_loop(N-1, List2);
                false ->  
                        %choose the first car from the list, kill and remove it 
                        case length(List) of 
                            12 ->
                                PID_F2 = lists:nth(7, List),
                                exit(PID_F2, kill),
                                List3 = [ {PIDF, PIDS} || {PIDF, PIDS} <- List, PIDF =/= PID_F2],
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
