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

%%	value(+Key:atom, +Grammar:term, -Value:list) is semidet.
%%	value(+Key:atom, +Grammar:term, ?Value:list) is det.
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

tables(Grammar, Tables, [Name-Size | Rest]) :-
    number(Size),
    functor(Table, Name, Size),
    value(Name, Grammar, Items),
    forall(
        member(Item, Items),
        (   item:value(index, Item, Index),
            Index1 is Index+1,
            (    item:entries(Item, Entries)
            ->   item:update_entries(Item, Entries, Item1)
            ;    Item1 = Item
            ),
        % if only using setarg, it will get out of local stack
            nb_linkarg(Index1, Table, Item1)
        )
          ),
    table:index(Name, TableIndex),
    TableIndex1 is TableIndex + 1,
    nb_linkarg(TableIndex1, Tables, Table),
    tables(Grammar, Tables, Rest).








