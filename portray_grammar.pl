:- module(portray_grammar, []).

:- use_module(library(assoc)).
:- use_module(grammar, []).
:- use_module(item, []).

:- multifile user:message_hook/3.

:- multifile user:portray/1.

user:portray(lexer_step(LexerType, _P0, Symbol-Data)) :-
    format('~p\t~p-~p~n', [LexerType, Symbol, Data]).

user:portray(parser_step(_P0, _P1, Action, Value, Lalr)) :-
    %table:item(lalr_table, Tables, Target, Lalr),
    format('~p  \t~p\t~p~n', [Action, Value, Lalr]).

user:portray(Grammar) :-
    Grammar = grammar(header(Header), _Assoc),
    grammar:properties(Grammar, Props),
    forall(member(Property, Props),
          (   item:get(name, Property, Name),
              item:get(value, Property, Value),
              format(' ~w: ~w', [Name, Value])
          )
          ),
    format(' version: ~w', [Header]).

user:portray(Tables) :-
    get_assoc(rule_table, Tables, _),
    get_assoc(symbol_table, Tables, _),
    write('_Tables').

user:portray(Assoc) :-
    assoc_to_list(Assoc, List),
    write('t'),
    print(List).

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

