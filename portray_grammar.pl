:- module(portray_grammar, []).

:- use_module(library(assoc)).

:- multifile user:message_hook/3.

:- multifile user:portray/1.

user:message_hook(Term, Kind, ['~p'-Term, nl]) :-
    format('t: ~p k: ~p', [Term, Kind]),
    Term = parser_step(_, _, _, _,_).

user:portray(parser_step(_P0, _P1, Action, Value, Lalr)) :-
    %table:item(lalr_table, Tables, Target, Lalr),
    format('~p\t~p\t~p~n', [Action, Value, Lalr]).

user:portray(parser(Grammar, _Tables)) :-
    Grammar = grammar(header(Header), _Assoc),
    format('version(~w)', [Header]).

user:portray(Assoc) :- is_assoc(Assoc).

%    get_assoc(table_counts, Assoc, [Counts]),
%    print(Counts).

%    ->  print_tables(Tables),
%        assoc_to_keys(Assoc, AssocKeys),
%        assoc_to_keys(Tables, TableKeys),
%        ord_subtract(AssocKeys, TableKeys, Other),
%        print_assoc_items(Assoc, Other)
%    ;   true
%    ).


user:portray('_entries'-Entries) :-
    Space = '        ',
    Entries =.. [entries | [First | Rest]],
    format('_entries: (~n~w~p', [Space, First]),
    forall(member(Entry, Rest), format(',~n~w~p', [Space, Entry])),
    format('~n~w)', [Space]).

user:portray(action-1) :- write(shift).
user:portray(action-2) :- write(reduce).
user:portray(action-3) :- write(goto).
user:portray(action-4) :- write(accept).

user:portray(kind-0) :- write(nonterminal).
user:portray(kind-1) :- write(terminal).
user:portray(kind-2) :- write(noise).
user:portray(kind-3) :- write(eof).
user:portray(kind-4) :- write(group_start).
user:portray(kind-5) :- write(group_end).
user:portray(kind-6) :- write(decremented).
user:portray(kind-7) :- write(error).

user:portray(Atom) :-
    atom(Atom),
    write_canonical(Atom).


%user:portray(Assoc) :-
%    assoc_to_list(Assoc, List),
%    format('t~p', [List]).

print_tables(Tables) :-
    forall(gen_assoc(KTable, Tables, VTable),
           (   VTable =.. [_ | VTableList],
               print_list_vertical(KTable, VTableList)
           )
          ).

print_assoc_items(Assoc, List) :-
    forall(member(Item, List),
           (   get_assoc(Item, Assoc, Values),
               print_list_vertical(Item, Values)
           )
          ).

print_list_vertical(Name, List) :-
    length(List, Length),
    format('~w: ~d~n', [Name, Length]),
    forall(member(Value, List), format('    ~p~n', [Value])).

