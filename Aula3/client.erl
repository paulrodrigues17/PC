-module(client).
-export([ls/1, get_file/2]).

ls(Server) -> 
    Server ! {self(), list_dir}, % vou mandar mensagem ao server a partir de mim (self())
    receive
        {Server, Files} -> 
            Files
    end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive
        {Server, Content} ->
            Content
    end.