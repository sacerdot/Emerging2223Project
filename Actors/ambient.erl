%Un attore ambiente omniscente che rappresenta lo stato reale del mondo. In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.
%
-module(ambient).
	-export([main/2, ambient/1]).

	ambient(Chessboard) -> 
		receive
			{isFree, PID, X, Y, Ref} -> %Request a car sends to the ambient 
				io:format("Request from ~p for parking (~p,~p)~n",[PID,X,Y]), %DEBUG
				PID ! {status, Ref, lists:member({X, Y, undefined}, Chessboard)}, %Reply to the car 
				io:format("Parking (~p,~p) is free: ~p~n", [X,Y, lists:member({X, Y, undefined}, Chessboard)]); %DEBUG
			{park, PID, X, Y, Ref} -> io:fwrite("DEFAULT\n"); %Update parkings  
				%{X, Y, undefined } -> {X,Y,PID} Substitution to be done in the list
			{leave, PID, Ref} -> io:fwrite("DEFAULT2\n"); %Update parkings
				%{_,_, PID} -> {_,_, undefined} Substitution to be done in the list
			_ -> io:fwrite("DEFAULT3\n")
		end,
		ambient(Chessboard). % Loop TODO: check if it's ok here or to be setted at the end of each receive block
	
	print_list([])-> ok;
	print_list([H|T]) -> 
		io:format("printing: ~p~n", [H]),
		print_list(T).
	
	main(H, W) ->
		Chessboard = [{X, Y, undefined} || X <- lists:seq(1, H), Y <- lists:seq(1, W)], %define a chessboard
		print_list(Chessboard),
		PIDA = spawn(?MODULE, ambient, [Chessboard]), %spawn the ambient actor
		io:format("Ambient PID: ~p~n", [PIDA]). %DEBUG
		%PIDA ! {isFree, self(), 1, 1, 1}. %DEBUG