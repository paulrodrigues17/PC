-module(library).
-author("Paul Rodrigues up201805428").
-import(lists, [foreach/2]).

%% API
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

%% Records
-record(people, {numCC, name, phone, address}).
-record(book, {bId, name, authors}).
-record(requisitions, {people, book}).


init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(people,       [{attributes, record_info(fields, people)}]),
    mnesia:create_table(book,         [{attributes, record_info(fields, book)}]),
    mnesia:create_table(requisitions, [{attributes, record_info(fields, requisitions)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([people,book,requisitions], 20000).

example_tables() ->
    [%% The people table
        {people, 1, joaquim,  9636, {"rua de faro"}},
        {people, 2, josefina, 9685, {"rua de madeira"}},
     %% The book table
        {book, 1000, "Uma gaiola de ouro",    ["Camilla Lackberg"]},
        {book, 1001, "Os Lusiadas",           ["Camoes"]},
        {book, 1002, "Os Lusiadas",           ["Camoes"]},
        {book, 1003, "Os Maias",              ["Eça de Queiros"]},
        {book, 1004, "Livro do Desassossego", ["Fernando Pessoa"]},
     %% The requisition table
        {requisitions, 1, [1000, 1002]},
        {requisitions, 2, [1001]}
    ].

reset_tables() ->
    mnesia:clear_table(people),
    mnesia:clear_table(book),
    mnesia:clear_table(requisitions),
    F = fun() ->
		    foreach(fun mnesia:write/1, example_tables())
	    end,
    mnesia:transaction(F).

% ----------------------------------------- Funções auxiliares ------------------------------ %
nrRequisitions([Length]) -> Length;
nrRequisitions([])       -> 0.

bookRequested([true|_]) -> true;
bookRequested([false|R]) -> bookRequested(R);
bookRequested([])    -> false.
% ----------------------------------------- Setters ----------------------------------------- %
add_people(NumCC, Name, Phone, Address) ->
    F = fun() ->
        mnesia:write(#people {numCC = NumCC,
                              name = Name,
                              phone = Phone,
                              address = Address
        })
        end,
    mnesia:transaction(F).

add_book(Bid, Name, Authors) ->
    F = fun() ->
        mnesia:write(#book {bId = Bid,
                            name = Name,
                            authors = Authors
        })
        end,
    mnesia:transaction(F).
% ---------------------------------------- MENSAGENS --------------------------------------- %
% ----------------------------------------- LOOKUP ----------------------------------------- %
book(NumCC) -> 
    do(qlc:q([ Y#book.name || X <- mnesia:table(requisitions), Y <- mnesia:table(book), 
               X#requisitions.people =:= NumCC, lists:member(Y#book.bId, X#requisitions.book)])
    ).

loan(BookName) ->
    do(qlc:q([ Y#people.name || X <- mnesia:table(book), Y <- mnesia:table(people), Z <- mnesia:table(requisitions),
               lists:member(X#book.bId, Z#requisitions.book), Z#requisitions.people =:= Y#people.numCC, X#book.name =:= BookName])
    ).

book_is_Requested(BookId) ->
    bookRequested(do(qlc:q([ lists:member(BookId, X#requisitions.book) || X <- mnesia:table(requisitions) ])
    )).

code(BookName) ->
    do(qlc:q([ X#book.bId || X <- mnesia:table(book),
               X#book.name =:= BookName ])
    ).

list_nrRequisitions(NumCC) ->
    nrRequisitions(do(qlc:q( [length(X#requisitions.book) || X <- mnesia:table(requisitions), 
               X#requisitions.people =:= NumCC])
    )).

do(Q) ->
    F = fun() -> 
        qlc:e(Q) 
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
% ----------------------------------------- MENSAGENS --------------------------------------- %
% ------------------------------------------ UPDATE ----------------------------------------- %
add_requisitions(NumCC, Bid) ->
    F = fun() ->
        [R] = mnesia:read(requisitions, NumCC, write),
        Book = lists:append(R#requisitions.book, [Bid]),
        New = R#requisitions{book = Book},
        mnesia:write(New)
        end,
    mnesia:transaction(F).

delete_requisitions(NumCC, Bid) ->
    F = fun() ->
        [R] = mnesia:read(requisitions, NumCC, write),
        Book = lists:delete(Bid, R#requisitions.book),
        New = R#requisitions{book = Book},
        mnesia:write(New)
    end,
    mnesia:transaction(F).

% Para ver as alterações em cima
select_requisitions(NumCC) ->
    list(do(qlc:q([ X#requisitions.book || X <-mnesia:table(requisitions), 
               X#requisitions.people =:= NumCC ])
    )).

list([R]) -> R;
list([]) -> [].