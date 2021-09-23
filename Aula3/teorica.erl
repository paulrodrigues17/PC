-module(teorica).
-export([rpc/4, start/1]).

start(Node) ->
    spawn(Node, fun() -> loop() end).

rpc(Pid, M, F, A) ->
    Pid ! {rpc, self(), M, F, A},
    receive
    {Pid, Response} ->
        Response
    end.

loop() ->
    receive
    {rpc, Pid, M, F, A} ->
        Pid ! {self(), (catch apply(M, F, A))},
        loop()
    end.

% programa onde podemos usar dois terminais 
% para ver onde está um determinado ficheiro
% podemos ver quais ficheiros estão nesse diretório com 
% list_dir 