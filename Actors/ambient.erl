%Un attore ambiente omniscente che rappresenta lo stato reale del mondo. In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.
%
-module(ambient).
	-export([main/2, ambient/1]).

	ambient(Chessboard) -> 
		receive
			{isFree, PID, X, Y, Ref} -> %Request a car sends to the ambient 
				io:format("Request from ~p for parking (~p,~p)~n",[PID,X,Y]), %DEBUG
				PID ! {status, Ref, undefined =:= dict:fetch({X,Y}, Chessboard)}, %Reply to the car 
				io:format("Parking (~p,~p) is free: ~p~n", [X,Y, undefined =:= dict:fetch({X,Y}, Chessboard)]), %DEBUG
				ambient(Chessboard);
			{park, PID, X, Y, Ref} -> 
				io:fwrite("Update Value for park ~p occupied by ~p~n", [{X,Y}, PID]), %Update parkings  
				ambient(dict:store({X,Y}, PID, Chessboard));
			{leave, PID, Ref} -> 
				io:fwrite("PID ~p exit from parking ~n", [PID]), %Update parkings
				case searchKey(Chessboard, PID) of
					{X, Y} -> 
						io:fwrite("Update Value for leave ~p free~n", [{X,Y}]), %Update parkings
						ambient(dict:store({X,Y}, undefined, Chessboard));
					[] -> 
						io:fwrite("PID: ~p not parked before ~n", [PID]),
						ambient(Chessboard)
				end;
			_ -> io:fwrite("DEFAULT3\n")
		end.
	
	%%%%%%
	%@params Dict: dict to print
	%Function to print each element of dict 
	%@dev: the fold function is used to iterate over the dict, the order of the elements is not guaranteed
	%       fold function takes as parameters a function, an accumulator and a dict
	printDict(Dict) -> 
		io:fwrite("Dict:~n"),
		dict:fold(fun(K, V, Acc) -> io:fwrite("Key ~p : Value: ~p~n", [K, V]), Acc end, [], Dict).

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

	main(H, W) ->
		Chessboard = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(1, H), Y <- lists:seq(1, W)]),
		printDict(Chessboard), %DEBUG
		io:format("Chessboard size ~p~n", [dict:size(Chessboard)]),
		PID_A = spawn(?MODULE, ambient, [Chessboard]), %spawn the ambient actor
		io:format("Ambient PID: ~p~n", [PID_A]), %DEBUG
	    register(ambient, PID_A), %register the ambient actor with the name ambient
	    io:format("Correctly registered ~p as 'ambient' ~n", [PID_A]), %DEBUG
		R = spawn(render, main, []),
		%register(render, R),
		R ! {Chessboard}. %DEBUG
