-module(car).
    -export([main/0]).

    %The main actor creates other actors and re-creates them if they fail
    main() ->
        F = spawn().

    friendship() ->
        io:fwrite("CIAO\n").

    state() ->
        io:fwrite("CIAO\n").

    detect() ->
        io:fwrite("CIAO\n").


    

