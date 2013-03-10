:- module(item, [
                 empty/1,
                 get/3,
                 set/4,
                 get_entries/2,
                 set_entries/3,
                 entries_to_list/2,
                 entry_member/3,
                 entries/3,
                 entry_size/2,
                 merge_keep/3,
                 merge_replace/3
                ]).

:- use_module(library(assoc)).

empty(Item) :-
    empty_assoc(Item).

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

%%	entries_to_list(+EntryTerm, ?Entries:list) is det.
% converts the term entry to a list of entries (using univ).
% succeeds if entries is already a list.
entries_to_list(Entries, Entries) :-
    is_list(Entries), !.
entries_to_list(EntryTerm, Entries) :-
    EntryTerm =.. [entries | Entries].

entry_member(Entries, Entry, EntryIndex) :-
    arg(ArgIndex, Entries, Entry),
    EntryIndex is ArgIndex - 1.

entries(Item, Entry, EntryIndex) :-
    get_entries(Item, Entries),
    entry_member(Entries, Entry, EntryIndex).

entry_size(Entries, Size) :-
    functor(Entries, entries, Size), !.

entry_size(Entries, Size) :-
    length(Entries, Size), !.

merge_keep(Key-Value, Item0, ItemN) :-
    (   get(Key, Item0, _Keep)
    ->  ItemN = Item0
    ;   set(Key, Item0, Value, ItemN)
    ).

merge_replace(Key-Value, Item0, ItemN) :-
    set(Key, Item0, Value, ItemN).

