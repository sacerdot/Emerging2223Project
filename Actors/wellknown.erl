%
-module(wellknown).
-export([main/0, wellknown/1]).

%%%%%%%%
%@param PIDSLIST: list of tuples {PIDF, PIDS} where PIDF is the PID of friendship actor and PIDS is the PID of state actor.
%                 Represents all the car that wellknown knows about. Initially it is empty
wellknown(PIDSLIST) ->
    receive
        %PID1 is the PID of friendship actor, PID2 is the PID of state actor
        {getFriends, PID1, PID2, Ref} ->
            %io:format("WK: getFriends received from PID: ~p~n", [PID1]),
            %Create a new list that contains all elements form PIDSLIST except the one that makes the request cause no one is friend with himself
            PIDSLIST2 = [ {PIDF, PIDS} || {PIDF, PIDS} <- PIDSLIST, PIDF =/= PID1],
            PID1 ! {myFriends, PIDSLIST2, Ref},
            %io:format("WK: Sent myFriends to PID: ~p, myFriends:~p~n", [PID1, PIDSLIST]),
                case lists:member({PID1, PID2}, PIDSLIST) of 
                %If the PID is already in my list
                true ->  wellknown(PIDSLIST);
                false ->
                    %Monitor the friendship actor of the Car I added to my list so I can remove it if he dies
                    monitor(process, PID1), 
                    %Add the new actor to the list
                    wellknown([{PID1, PID2}|PIDSLIST])
            end;
        %Case a PID I monitor dies
        {'DOWN', _, _, PID_Friendship, _Reason} ->
           %io:format("WK: Delete PID: ~p for reason: ~p~n", [PID_Friendship, _Reason]),
           %Create a new list that contains all elements form PIDSLIST except the one that died
           UpdatedList = [ {PIDF, PIDS} || {PIDF, PIDS} <- PIDSLIST, PIDF =/= PID_Friendship],
           wellknown(UpdatedList)
    end.


%%%%%
% Main of wellknown actor it spawns the wellknown actor and registers it with the name wellknown
%@return PID_W : PID of wellknown actor 
main() ->
    PID_W = spawn(?MODULE, wellknown, [[]]),
    register(wellknown, PID_W),
    io:format("WN: Wellknown started with PID ~p registered with wellknown~n", [PID_W]),
    PID_W.
   
