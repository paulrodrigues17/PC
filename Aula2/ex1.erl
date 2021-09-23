-module(aula2).
-export([compute_factorial/1, start/0, factorial/2]).

% 1

compute_factorial(0) -> 1;
compute_factorial(N) -> N * compute_factorial(N-1).

loop() -> 
    receive
        % comando factorial
        {factorial, From, M} -> From ! {response, compute_factorial(M)},
        loop()
    end.

 start() -> spawn(fun() -> loop() end).

factorial(Server, M) -> 
    % enviar um request ao server
    Server ! {factorial, self(), M},
    receive {response, Result} -> Result end.