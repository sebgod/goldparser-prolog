:- module(state, [
                  current/2,
                  current_list/2,
                  merge/3
                 ]).

:- use_module(table, []).
:- use_module(item, []).
:- use_module(library(assoc)).

current(State, Kind-KindState) :-
    get_assoc(Kind, State, KindState).

current_list(State, List) :-
    maplist(current(State), List).

merge_replace(Key-Value, Item0, ItemN) :-
    put_assoc(Key, Item0, Value, ItemN).

merge(State, Updates, NewState) :-
    foldl(merge_replace, Updates, State, NewState).


