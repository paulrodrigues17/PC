-module(ex1).
-import(lib_misc, [unconsult/2]).
-compile(export_all).

member(X,[X|R]) -> true;
member(X,[Y|R]) -> member(X,R);
member(X,[])    -> false. 

sum([X|R]) -> X + sum(R);
sum([])    -> 0.

reverse(L) -> reverseAux(L,[]).
reverseAux([X|R],L) -> reverseAux(R,[X|L]);
reverseAux([], L)   -> L.

map(F,[X|R]) -> [F(X)|map(F,R)];
map(F,[])    -> [].

dobro(X) -> 2*X.

partition(Pred, List) -> part(Pred,List,[],[]).
part(Pred,[],T,F) -> {T,F};
part(Pred,[H|R],T,F) -> 
    case Pred(H) of
        true -> part(Pred,R,[H|T],F);
        false -> part(Pred,R,T,[H|F])
    end.

par(X) -> X rem 2 =:= 0.
impar(X) -> X rem 2 =/= 0.

update(File, Key, Delta) -> 
    {ok, Terms} = file:consult(File),
    Terms1 = do_update(Key, Delta, Terms),
    unconsult(File ++ ".tmp", Terms1).

do_update(Key, Delta, [{Key,Val}|T]) ->
    [{Key,Val+Delta}|T];
do_update(Key,Delta,[H|T]) -> [H|do_update(Key,Delta,T)];
do_update(Key,Delta,[]) -> [{Key,Delta}].