-module(ex3).
-export([compute_factorial/1, start/0, factorial/2, stop/1, status/1]).

compute_factorial(0) -> 1;
compute_factorial(N) -> N * compute_factorial(N-1).

factorial(Server, M) ->
    Ref = make_ref(), 
    % enviar um request ao server
    Server ! {factorial, self(), Ref, M},
    receive {response, Ref, Result} -> Result end.

start() -> spawn(fun() -> loop(0) end).

stop(Server) -> 
    Server ! {stop, self()}, 
    ok.

status(Server) ->
    Ref = make_ref(), % função buit-in que gera uma nova referência 
    % o cliente Ref envia um pedido ao server 
    Server ! {status, self(), Ref}, 
    % espera pela resposta e retorna-a
    receive {response, Ref, Result} -> Result end.

loop(N) -> 
    receive
        % comando factorial 
        {factorial, From, Ref, M} -> 
            From ! {response, Ref, compute_factorial(M)}, loop(N+1);
        % comando status 
        {status, From, Ref} ->
            From ! {response, Ref, N}, 
            loop(N);
        {stop, From, Ref} -> 
            ok
    end.
