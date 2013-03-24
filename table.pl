:- module(table, [
                  index/2,
                  by_name/3,
                  item/3,
                  item/4,
                  items/4,
                  size/3
                 ]).

index(character_set_table, 1).
index(dfa_table, 2).
index(group_table, 3).
index(lalr_table, 4).
index(properties, 5).
index(rule_table, 6).
index(symbol_table, 7).

by_name(Name, Tables, Table) :-
    get_assoc(Name, Tables, Table).

item(Name, Tables, Index, Item) :-
    by_name(Name, Tables, Table),
    item(Table, Index, Item).

item(Table, Index, Item) :-
    TableIndex is Index + 1,
    arg(TableIndex, Table, Item).

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
