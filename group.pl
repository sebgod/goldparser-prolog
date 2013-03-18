:- module(group, [by_symbol/4]).

:- use_module(table,  []).
:- use_module(item,   []).

by_symbol(Tables, Kind-SymbolIndex, GroupIndex, Group) :-
    table:items(group_table, Tables, GroupIndex, Group),
    item:get(Kind, Group, SymbolIndex).
