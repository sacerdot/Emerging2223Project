%
-module(wellknown).
-export([main/0, wellknown/1]).


wellknown(PIDSLIST) ->
    receive
        %PID1 is the PID of friendship actor, PID2 is the PID of state actor
        {getFriends, PID1, PID2, Ref} ->
            PID1 ! {myFriends, PIDSLIST, Ref},
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
           io:format("WK: Delete PID: ~p for reason: ~p~n", [PID_Friendship, _Reason]),
           %Create a new list that contains all elements form PIDSLIST except the one that died
           UpdatedList = [ {PIDF, PIDS} || {PIDF, PIDS} <- PIDSLIST, PIDF =/= PID_Friendship],
           wellknown(UpdatedList)
    end.

main() ->
    PID_W = spawn(?MODULE, wellknown, [[]]),
    register(wellknown, PID_W),
    io:format("WN: Wellknown started with PID ~p registered with wellknown~n", [PID_W]).
