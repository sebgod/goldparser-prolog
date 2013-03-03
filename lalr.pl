:- module(lalr, [current/3]).

:- use_module(state).

current(Tables, State, Lalr) :-
    state:current_item(Tables, State, lalr-lalr_table, Lalr).
