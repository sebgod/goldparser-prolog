:- module(parser_members, [
						   table_item/4,
						   table_items/4,
						   table_size/3,
						   entries/2,
						   entries_iterate/3
						  ]).

table_item(Name, Tables, Index, Item) :-
	get_assoc(Name, Tables, Table),
	TableIndex is Index + 1,
	arg(TableIndex, Table, Item).

table_items(Name, Tables, Index, Item) :-
	table_size(Name, Tables, Size),
	LastIndex is Size -1,
	between(0, LastIndex, Index),
	table_item(Name, Tables, Index, Item).

table_size(Name, Tables, Size) :-
	get_assoc(Name, Tables, Table),
	functor(Table, Name, Size).

entries(Item, Entries) :-
	get_assoc('_entries', Item, Entries).

entries_iterate(Entries, Entry, EntryIndex) :-
	arg(EntryIndex, Entries, Entry).
