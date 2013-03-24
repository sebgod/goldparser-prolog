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
:- use_module(item, []).

value(Key, grammar(_H, Grammar), Value) :-
    get_assoc(Key, Grammar, Value).

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
    empty_assoc(Tables0),
    %empty_tables(Tables0, CountsWithProperties),
    tables(Grammar, CountsWithProperties, Tables0, Tables).

tables(_Grammar, [], T, T).

tables(Grammar, [Name-Size | Rest], Tables0, Tables) :-
    number(Size),
    functor(Table, Name, Size),
    put_assoc(Name, Tables0, Table, Tables1),
    (   value(Name, Grammar, Items)
    ->  forall(
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
              )
    ;   true
    ),
    tables(Grammar, Rest, Tables1, Tables).







