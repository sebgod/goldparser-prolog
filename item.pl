:- module(item, [
                 empty/1,
                 value/3,
                 entries/2,
                 update_value/4,
                 update_entries/3,
                 entries_to_list/2,
                 entry_member/2,
                 entries_nd/2,
                 entry_size/2,
                 merge_keep/3,
                 merge_replace/3
                ]).

:- use_module(library(assoc)).

empty(Item) :-
    empty_assoc(Item).

value(Name, Item, Value) :-
    get_assoc(Name, Item, Value).

update_value(Name, Item, Value, NewItem) :-
    put_assoc(Name, Item, Value, NewItem).

entries(Item, Entries) :-
    value('_entries', Item, Entries).

update_entries(Item, Entries, NewItem) :-
    (   functor(Entries, entries, _)
    ->  put_assoc('_entries', Item, Entries, NewItem)
    ;   must_be(list, Entries),
        EntryTerm =.. [entries | Entries],
        update_entries(Item, EntryTerm, NewItem)
    ).

%%	entries_to_list(+EntryTerm:term, ?Entries:list) is det.
% converts the term entry to a list of entries (using univ).
% succeeds if entries is already a list.
entries_to_list(Entries, Entries) :-
    is_list(Entries), !.
entries_to_list(EntryTerm, Entries) :-
    EntryTerm =.. [entries | Entries].

entry_member(Entries, Entry) :-
    entries_to_list(Entries, EntryList),
    member(Entry, EntryList).

entries_nd(Item, Entry) :-
    entries(Item, Entries),
    entry_member(Entries, Entry).

entry_size(Entries, Size) :-
    functor(Entries, entries, Size), !.

entry_size(Entries, Size) :-
    length(Entries, Size), !.

merge_keep(Key-Value, Item0, ItemN) :-
    (   value(Key, Item0, _Keep)
    ->  ItemN = Item0
    ;   update_value(Key, Item0, Value, ItemN)
    ).

merge_replace(Key-Value, Item0, ItemN) :-
    update_value(Key, Item0, Value, ItemN).









