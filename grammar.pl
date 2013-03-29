:- module(grammar, [
                    value/3,
                    counts/2,
                    properties/2,
                    initial_states/2,
                    %items_to_list/2,
                    %empty_tables/1,
                    tables/2
                   ]).

:- use_module(library(assoc)).
:- use_module(table, []).
:- use_module(item, []).

%%	value(+Key:atom, +Grammar:term, -Value:list) is det.
%%	value(+Key:atom, +Grammar:term, ?Value:list) is semidet.
% retrieves the value for a given key from the grammar.
% Currently, the grammar is using the AVL (in SWI-Prolog)
% from library(assoc).
% If the key is not found, Value is unified with the empty list.
value(Key, grammar(_H, Grammar), Value) :-
    get_assoc(Key, Grammar, Value), !.

value(_Key, _Grammar, []).

counts(Grammar, Counts) :-
    value(table_counts, Grammar, [Counts]).

properties(Grammar, Properties) :-
    value(property, Grammar, Properties).

initial_states(Grammar, State) :-
    value(initial_states, Grammar, [State]).

items_to_list(Items, ItemsList) :-
    assoc_to_list(Items, ItemsList).

empty_tables(Tables, TableCounts) :-
    length(TableCounts, NumberOfTables),
    functor(Tables, tables, NumberOfTables).

tables(Grammar, Tables) :-
    counts(Grammar, Counts),
    properties(Grammar, Properties),
    length(Properties, NumberOfProperties),
    items_to_list(Counts, CountsList),
    CountsWithProperties = [property-NumberOfProperties | CountsList],
    empty_tables(Tables, CountsWithProperties),
    tables(Grammar, Tables, CountsWithProperties).

tables(_Grammar, _, []).

tables(Grammar, Tables, [TableName-Size | Rest]) :-
    number(Size),
    functor(Table, TableName, Size),
    value(TableName, Grammar, GrammarItems),
    forall(
        member(GrammarItem, GrammarItems),
        (   item_to_term(TableName, GrammarItem, TableItem),
            item:value(index, TableItem, Index),
            Index1 is Index+1,
        % if only using setarg, it will get out of local stack
            nb_linkarg(Index1, Table, TableItem)
        )
          ),
    table:index(TableName, TableIndex),
    TableIndex1 is TableIndex + 1,
    nb_linkarg(TableIndex1, Tables, Table),
    tables(Grammar, Tables, Rest).

item_to_term(TableName, GrammarItem, TableItem) :-
    item:property(TableName, _, _, _, TableItem, _),
    forall(item:property(TableName, Key, Index, _, _, _),
           (   Arg is Index + 1,
               get_assoc(Key, GrammarItem, Value),
               nb_linkarg(Arg, TableItem, Value)
           )
          ),
    entries_to_term(TableName, GrammarItem, TableItem).

entries_to_term(_TableName, _GrammarItem, TableItem) :-
    ground(TableItem), !.

entries_to_term(TableName, GrammarItem, TableItem) :-
    (   get_assoc('_entries', GrammarItem, Entries)
    ->  length(Entries, NumberOfEntries),
        functor(EntriesTerm, entries, NumberOfEntries),
        foldl(entry_to_term(TableName, EntriesTerm), Entries, 1, _)
    ;   EntriesTerm = entries
    ),
    functor(TableItem, _, Arity),
    nb_linkarg(Arity, TableItem, EntriesTerm).

entry_to_term(TableName, EntriesTerm, EntryAssoc, EntryIndex, EntryIndex1) :-
    EntryIndex1 is EntryIndex + 1,
    item:entry_value(TableName, _, _, _, EntryTerm, _),
    forall(item:entry_value(TableName, Key, Index, _, _, _),
           (   Arg is Index + 1,
               get_assoc(Key, EntryAssoc, Value),
               nb_linkarg(Arg, EntryTerm, Value)
           )
          ),
    nb_linkarg(EntryIndex, EntriesTerm, EntryTerm).


