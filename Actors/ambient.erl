%Un attore ambiente omniscente che rappresenta lo stato reale del mondo. In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.
%
-module(ambient).
-export([main/2, ambient/1]).
-define(W, 5).

	ambient(Chessboard) -> 
		receive
			{isFree, PID, X, Y, Ref} -> %Request a car sends to the ambient 
				io:format("A: Request from ~p for parking (~p,~p)~n",[PID,X,Y]), %DEBUG
				io:format("A: Parking (~p,~p) is free: ~p~n", [X,Y, undefined =:= dict:fetch({X,Y}, Chessboard)]), %DEBUG
				io:format("A: Reply to ~p with Ref ~p~n", [PID, Ref]), %DEBUG
				PID ! {status, Ref, undefined =:= dict:fetch({X,Y}, Chessboard)}, %Reply to the car 
				ambient(Chessboard);
			{park, PID, X, Y, Ref} -> 
				io:fwrite("A: Update Value for park ~p occupied by ~p~n", [{X,Y}, PID]), %Update parkings  
				ambient(dict:store({X,Y}, PID, Chessboard));
			{leave, PID, Ref} -> 
				io:fwrite("A: PID ~p exit from parking ~n", [PID]), %Update parkings
				case searchKey(Chessboard, PID) of
					{X, Y} -> 
						io:fwrite("A: Update Value for leave ~p free~n", [{X,Y}]), %Update parkings
						ambient(dict:store({X,Y}, undefined, Chessboard));
					[] -> 
						io:fwrite("A: PID: ~p not parked before ~n", [PID]),
						ambient(Chessboard)
				end;
			draw -> 
				io:fwrite("A: Entered Draw ~n"), %DEBUG
				render ! {dict:to_list(Chessboard)},
				ambient(Chessboard);                  %DEBUG
			_ -> io:fwrite("A: No Pattern Matching Found!\n")
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
		%Chessboard Definition
		Chessboard = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)]),
		printDict(Chessboard), %DEBUG
		io:format("Chessboard size ~p~n", [dict:size(Chessboard)]),
		
		%Spawn the render actor
		PID_R = spawn(render, main, [[{X, Y} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)], dict:new()]),
		register(render, PID_R),
		io:format("Correctly registered ~p as 'render' ~n", [PID_R]), %DEBUG
		render ! {dict:to_list(Chessboard)}, %DEBUG
		
		%Spawn ambient actor 
		PID_A = spawn(?MODULE, ambient, [Chessboard]), %spawn the ambient actor
		io:format("Ambient PID: ~p~n", [PID_A]), %DEBUG
	    register(ambient, PID_A), %register the ambient actor with the name ambient
	    io:format("Correctly registered ~p as 'ambient' ~n", [PID_A]). %DEBUG
