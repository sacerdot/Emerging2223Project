-module(ambient).
-export([main/1, ambient/1]).

	%%%%%
	% Ambient actor, an omniscient actor that represents the real state of the world. In particular, 
	% the ambient knows for each cell/parking its state (free or occupied).
	% @param Chessboard: dict that represents the state of the world. The key is a tuple {X,Y} that represents the position of the parking
	ambient(Chessboard) -> 
		receive
			{isFree, PID, X, Y, Ref} -> %Request a car sends to the ambient 
				%io:format("AMB: Request from ~p for parking (~p,~p)~n",[PID,X,Y]), %DEBUG
				%io:format("AMB: Parking (~p,~p) is free: ~p~n", [X,Y, undefined =:= dict:fetch({X,Y}, Chessboard)]), %DEBUG
				%io:format("AMB: Reply to ~p with Ref ~p~n", [PID, Ref]), %DEBUG
				PID ! {status, Ref, undefined =:= dict:fetch({X,Y}, Chessboard)}, %Reply to the car 
				ambient(Chessboard);
			{park, PID, X, Y, Ref} -> 
				%io:fwrite("AMB: Update Value for park ~p occupied by ~p~n", [{X,Y}, PID]), %Update parkings  
				render ! {parked, PID, X, Y, true}, %DEBUG
				ambient(dict:store({X,Y}, PID, Chessboard));
			{leave, PID, Ref} -> 
				%io:fwrite("AMB: PID ~p exit from parking ~n", [PID]), %Update parkings
				case searchKey(Chessboard, PID) of
					{X, Y} -> 
						%io:fwrite("AMB: Update Value for leave ~p free~n", [{X,Y}]), %Update parkings
						render ! {parked, PID, X, Y, false},
						ambient(dict:store({X,Y}, undefined, Chessboard));
					[] -> 
						%io:fwrite("AMB: PID: ~p not parked before ~n", [PID]),
						ambient(Chessboard)
				end;
			_ -> io:fwrite("AMB: No Pattern Matching Found!\n")
		end.
	
	%%%%%%
	%@params Dict: dict to search
	%@params Value: value used search the key
	%@return Key: key of the dict that has the value passed as parameter
	%@dev: this function can return a key {X,Y} if the value exist in the dict or [] if the value doesn't exist
    searchKey(Dict, Value) ->
        dict:fold(fun(K, V, Acc) -> 
						case V of 
							Value -> K; 
							_ -> Acc 
						end
					end, [], Dict).

	%%%%%
	% Main of ambient actor it spawns the ambient actor and registers it with the name ambient
	% @return PID_A : PID of ambient actor 
	main(Chessboard) ->
		PID_A = spawn(?MODULE, ambient, [Chessboard]), %spawn the ambient actor
		register(ambient, PID_A), %register the ambient actor with the name ambient
		io:format("AMBIENT: Correctly registered ~p as 'ambient' ~n", [PID_A]), %DEBUG
		PID_A.
		
		