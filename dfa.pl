:- module(dfa,
          [
           accept/2,
           find_edge/4
          ]).

:- use_module(item, []).
:- use_module(table, []).
:- use_module(state, []).
:- use_module(charset, []).

accept(DFA, Accept) :-
    (    item:value(accept_state, DFA, true)
    ->   item:value(accept_index, DFA, Accept)
    ;    Accept = none
    ).

find_edge(Charsets, DFA, Code, TargetIndex) :-
    item:entries_nd(DFA, Edge),
    item:value(target_index, Edge, TargetIndex),
    item:value(character_set_index, Edge, CharsetIndex),
    table:item(Charsets, CharsetIndex, Charset),
    charset:member(Charset, Code) -> !.
