-module(aula4).
-import(lists, [foreach/2]).
-compile(export_all).


-include_lib("stdlib/include/qlc.hrl").
-include("aula4.hrl").

example_tables() ->
              [   {employee, 104465, "Johnson Torbjorn",   1, male,  99184, {242,038}},
                  {employee, 107912, "Carlsson Tuula",     2, female,94556, {242,056}},
                  {employee, 114872, "Dacker Bjarne",      3, male,  99415, {221,035}},
                  {employee, 104531, "Nilsson Hans",       3, male,  99495, {222,026}},
                  {employee, 104659, "Tornkvist Torbjorn", 2, male,  99514, {222,022}},
                  {employee, 104732, "Wikstrom Claes",     2, male,  99586, {221,015}},
                  {employee, 117716, "Fedoriw Anna",       1, female,99143, {221,031}},
                  {employee, 115018, "Mattsson Hakan",     3, male,  99251, {203,348}},
                  {dept, 'B/SF',  "Open Telecom Platform"},
                  {dept, 'B/SFP', "OTP - Product Development"},
                  {dept, 'B/SFR', "Computer Science Laboratory"},
                  {project, erlang, 1},
                  {project, otp, 2},
                  {project, beam, 3},
                  {project, mnesia, 5},
                  {project, wolf, 6},
                  {project, documentation, 7},
                  {project, www, 8},
                  {manager, 104465, 'B/SF'},
                  {manager, 104465, 'B/SFP'},
                  {manager, 114872, 'B/SFR'},
                  {at_dep, 104465, 'B/SF'},
        {at_dep, 107912, 'B/SF'},
        {at_dep, 114872, 'B/SFR'},
        {at_dep, 104531, 'B/SFR'},
        {at_dep, 104659, 'B/SFR'},
        {at_dep, 104732, 'B/SFR'},
        {at_dep, 117716, 'B/SFP'},
        {at_dep, 115018, 'B/SFP'},
        {in_proj, 104465, otp},
        {in_proj, 107912, otp},
        {in_proj, 114872, otp},
        {in_proj, 104531, otp},
        {in_proj, 104531, mnesia},
        {in_proj, 104545, wolf},
        {in_proj, 104659, otp},
        {in_proj, 104659, wolf},
        {in_proj, 104732, otp},
        {in_proj, 104732, mnesia},
        {in_proj, 104732, erlang},
        {in_proj, 117716, otp},
        {in_proj, 117716, documentation},
        {in_proj, 115018, otp},
        {in_proj, 115018, mnesia}
                ].


do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    init(),
    mnesia:stop().

init() ->
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)}]),
    mnesia:create_table(dept,
                        [{attributes, record_info(fields, dept)}]),
    mnesia:create_table(project,
                        [{attributes, record_info(fields, project)}]),
    mnesia:create_table(manager, [{type, bag},
                                  {attributes, record_info(fields, manager)}]),
    mnesia:create_table(at_dep,
                         [{attributes, record_info(fields, at_dep)}]),
    mnesia:create_table(in_proj, [{type, bag},
                                  {attributes, record_info(fields, in_proj)}]).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([employee,dept,project,manager,at_dep,in_proj], 20000).

reset_tables() ->
    mnesia:create_table(employee),
    mnesia:create_table(dept),
    mnesia:create_table(project),
    mnesia:create_table(manager),
    mnesia:create_table(at_dep),
    mnesia:create_table(in_proj),
    F = fun() -> 
        foreach(fun mnesia:write/1, example_tables())
        end,
    mnesia:transaction(F).

demo(Table) -> 
    do(qlc:q([ X || X <- mnesia:table(Table) ])
    ).

females() ->
    do(qlc:q([ X#employee.name || X <- mnesia:table(employee), 
               X#employee.sex =:= female])
    ).

idFemales() ->
    do(qlc:q([ X#employee.emp_no || X <- mnesia:table(employee), 
               X#employee.sex =:= female])
    ).

do(Q) ->
    F = fun() -> 
        qlc:e(Q) 
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

add_employee(Emp_no,Name,Salary,Sex,Phone,Room_no) -> 
    Elem = #employee{emp_no = Emp_no, name = Name, salary = Salary,
                     sex = Sex, phone = Phone, room_no = Room_no},
    F = fun() ->
            mnesia:write(Elem)
            end,
    mnesia:transaction(F).

raise(Empno, Raise) ->   
    F = fun() ->
            [E] = mnesia:read(employee, Empno, write),
            Salary = E#employee.salary + Raise,
            New = E#employee{salary = Salary},
            mnesia:write(New)
        end,
        mnesia:transaction(F).

raise_females(Amount) ->
    raise_list(idFemales(), Amount).

raise_list([E|Tail], Amount) ->
    raise(E,Amount),
    raise_list(Tail, Amount);

raise_list([], _) ->
    0.
