:- module(state, [
                  current/2,
                  current_list/2,
                  merge/3
                 ]).

:- use_module(table, []).
:- use_module(item, []).

current(State, Kind-KindState) :-
    item:get(Kind, State, KindState).

current_list(State, List) :-
    maplist(current(State), List).

merge(State, Updates, NewState) :-
    foldl(item:merge_replace, Updates, State, NewState).


