:- module(portray_grammar, []).

:- use_module(library(assoc)).

user:portray(parser(Grammar, Tables)) :-
    Grammar = grammar(header(Header), Assoc),
    format('Version: ~w~n', [Header]),
    print_tables(Tables),
    assoc_to_keys(Assoc, AssocKeys),
    assoc_to_keys(Tables, TableKeys),
    ord_subtract(AssocKeys, TableKeys, Other),
    print_assoc_items(Assoc, Other).

user:portray('_entries'-Entries) :-
    Space = '        ',
    Entries =.. [entries | [First | Rest]],
    format('_entries: (~n~w~p', [Space, First]),
    forall(member(Entry, Rest), format(',~n~w~p', [Space, Entry])),
    format('~n~w)', [Space]).

user:portray(action-1) :- write(action-shift).
user:portray(action-2) :- write(action-reduce).
user:portray(action-3) :- write(action-goto).
user:portray(action-4) :- write(action-accept).

user:portray(kind-0) :- write(kind-nonterminal).
user:portray(kind-1) :- write(kind-terminal).
user:portray(kind-2) :- write(kind-noise).
user:portray(kind-3) :- write(kind-eof).
user:portray(kind-4) :- write(kind-group_start).
user:portray(kind-5) :- write(kind-group_end).
user:portray(kind-6) :- write(kind-decremented).
user:portray(kind-7) :- write(kind-error).

user:portray(Atom) :-
    atom(Atom),
    write_canonical(Atom).

user:portray(Assoc) :-
    assoc_to_list(Assoc, List),
    format('t~p', [List]).

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
    (   Length > 0
    ->  forall(member(Value, List), format('    ~p~n', [Value]))
    ;   format('~w', [[]])
    ).

