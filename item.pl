:- module(item, [
                 get/3,
                 set/4,
                 get_entries/2,
                 set_entries/3,
                 entries_to_list/2,
                 entries/3,
                 entry_size/2
                ]).

get(Name, Item, Value) :-
    get_assoc(Name, Item, Value).

set(Name, Item, Value, NewItem) :-
    put_assoc(Name, Item, Value, NewItem).

get_entries(Item, Entries) :-
    get('_entries', Item, Entries).

set_entries(Item, Entries, NewItem) :-
    (   functor(Entries, entries, _)
    ->  put_assoc('_entries', Item, Entries, NewItem)
    ;   must_be(list, Entries),
        EntryTerm =.. [entries | Entries],
        set_entries(Item, EntryTerm, NewItem)
    ).

entries_to_list(Entries, Entries) :-
    is_list(Entries), !.
entries_to_list(EntryTerm, Entries) :-
    EntryTerm =.. [entries | Entries].

entries(Entries, Entry, EntryIndex) :-
    arg(ArgIndex, Entries, Entry),
    EntryIndex is ArgIndex - 1.

entry_size(Entries, Size) :-
    functor(Entries, entries, Size), !.

entry_size(Entries, Size) :-
    length(Entries, Size), !.

