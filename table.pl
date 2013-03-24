:- module(table, [
                  index/2,
                  by_index/3,
                  by_name/3,
                  item/3,
                  item/4,
                  items/4,
                  size/3
                 ]).

table(character_set_table, 0, tables(Table, _, _, _, _, _, _), Table).
table(dfa_table,           1, tables(_, Table, _, _, _, _, _), Table).
table(group_table,         2, tables(_, _, Table, _, _, _, _), Table).
table(lalr_table,          3, tables(_, _, _, Table, _, _, _), Table).
table(property,            4, tables(_, _, _, _, Table, _, _), Table).
table(rule_table,          5, tables(_, _, _, _, _, Table, _), Table).
table(symbol_table,        6, tables(_, _, _, _, _, _, Table), Table).

index(Name, Index) :- table(Name, Index, _, _).

by_index(Tables, Index, Table) :-
    table(_, Index, Tables, Table).

by_name(Name, Tables, Table) :-
    table(Name, _, Tables, Table).
%    get_assoc(Name, Tables, Table).

item(Name, Tables, Index, Item) :-
    by_name(Name, Tables, Table),
    item(Table, Index, Item).

item(Table, Index, Item) :-
    Index1 is Index + 1,
    arg(Index1, Table, Item).

items(Name, Tables, Index, Item) :-
    by_name(Name, Tables, Table),
    size(Table, Size),
    LastIndex is Size -1,
    between(0, LastIndex, Index),
    item(Table, Index, Item).

size(Name, Tables, Size) :-
    by_name(Name, Tables, Table),
    size(Table, Size).

size(Table, Size) :-
    functor(Table, _, Size).
