:- module(dfa,
          [
           current/3,
           accept/2,
           find_edge/4
          ]).

:- use_module(item, []).
:- use_module(table, []).
:- use_module(state, []).
:- use_module(charset, []).

current(Tables, State, DFA) :-
    state:current_item(Tables, State, dfa-dfa_table, DFA).

accept(DFA, Accept) :-
    (    item:get(accept_state, DFA, true)
    ->   item:get(accept_index, DFA, Accept)
    ;    Accept = none
    ).

find_edge(Tables, DFA, Code, TargetIndex) :-
    item:entries(DFA, Edge, _EntryIndex),
    item:get(target_index, Edge, TargetIndex),
    item:get(character_set_index, Edge, CharsetIndex),
    table:item(character_set_table, Tables, CharsetIndex, Charset),
    charset:member(Charset, Code) -> !.
