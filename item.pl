:- module(item, [
                 get/3,
                 set/4,
                 get_entries/2,
                 set_entries/3,
                 entries/3,
                 entry_size/2
                ]).

get(Name, Item, Value) :-
    get_assoc(Name, Item, Value).
get_entries(Item, Entries) :-
    get('_entries', Item, Entries).

set(Name, Item, Value, NewItem) :-
    put_assoc(Name, Item, Value, NewItem).

set_entries(Item, Entries, NewItem) :-
    (   functor(Entries, entries, _)
    ->  put_assoc('_entries', Item, Entries, NewItem)
    ;   must_be(list, Entries),
        EntriesTerm =.. [entries | Entries],
        set_entries(Item, EntriesTerm, NewItem)
    ).

entries(Entries, Entry, EntryIndex) :-
    arg(ArgIndex, Entries, Entry),
    EntryIndex is ArgIndex - 1.

entry_size(Entries, Size) :-
    functor(Entries, entries, Size), !.

entry_size(Entries, Size) :-
    length(Entries, Size), !.

