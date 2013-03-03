:- module(table, [get/3, item/4, items/4, size/3]).

get(Name, Tables, Table) :-
    get_assoc(Name, Tables, Table).

item(Name, Tables, Index, Item) :-
    get(Name, Tables, Table),
    TableIndex is Index + 1,
    arg(TableIndex, Table, Item).

items(Name, Tables, Index, Item) :-
    size(Name, Tables, Size),
    LastIndex is Size -1,
    between(0, LastIndex, Index),
    item(Name, Tables, Index, Item).

size(Name, Tables, Size) :-
    get(Name, Tables, Table),
    functor(Table, Name, Size).
