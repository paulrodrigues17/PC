-module(ex2).
-export([compute_factorial/1, start/0, factorial/2, stop/1, status/1]).

compute_factorial(0) -> 1;
compute_factorial(N) -> N * compute_factorial(N-1).

factorial(Server, M) -> 
    % enviar um request ao server
    Server ! {factorial, self(), M},
    receive {response, Result} -> Result end.

loop(N) ->
    receive 
        {factorial, From, M} ->
            From ! {response, compute_factorial(M)}, loop(N+1);
        {status, From} ->
            From ! {response, N}, 
            loop(N);
        {stop, From} ->
            ok
    end.

start() -> spawn(fun() -> loop(0) end).

stop(Server) -> 
    Server ! {stop, self()}, 
    ok.

status(Server) -> 
    Server ! {status, self()},
    receive {response, Result} -> Result end.