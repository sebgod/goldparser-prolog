:- module(entries, [list/2, iterate/3]).

list(Item, Entries) :-
	get_assoc('_entries', Item, Entries).

iterate(Entries, Entry, EntryIndex) :-
	arg(EntryIndex, Entries, Entry).
