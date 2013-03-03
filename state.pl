:- module(state, [
                  current_item/4,
                  current/2,
                  current_list/2,
                  merge/3
                 ]).

:- use_module(table, []).
:- use_module(item, []).

:- meta_predicate
    merge_(+, +, ?).

current_item(Tables, State, Name-Table, Item) :-
    current(State, Name-Index),
    table:item(Table, Tables, Index, Item).

current(State, Kind-KindState) :-
    item:get(Kind, State, KindState).

current_list(State, List) :-
    maplist(current(State), List).

merge(State, Updates, NewState) :-
    foldl(merge_, Updates, State, NewState).

merge_(Kind-KindState, State, NewState) :-
    item:set(Kind, State, KindState, NewState).


