:- module(table, [item/4, items/4, size/3]).

item(Name, Tables, Index, Item) :-
    get_assoc(Name, Tables, Table),
    TableIndex is Index + 1,
    arg(TableIndex, Table, Item).

items(Name, Tables, Index, Item) :-
    size(Name, Tables, Size),
    LastIndex is Size -1,
    between(0, LastIndex, Index),
    item(Name, Tables, Index, Item).

size(Name, Tables, Size) :-
    get_assoc(Name, Tables, Table),
    functor(Table, Name, Size).
