:- module(grammar, [
                    get/3,
                    get_counts/2,
                    get_properties/2,
                    get_initial_states/2,
                    items_to_list/2,
                    empty_tables/1,
                    create_tables/2
                   ]).

:- use_module(library(assoc)).

get(Key, grammar(_H, Grammar), Value) :-
    get_assoc(Key, Grammar, Value).

get_counts(Grammar, Counts) :-
    get(table_counts, Grammar, [Counts]).

get_properties(Grammar, Properties) :-
    get(property, Grammar, Properties).

get_initial_states(Grammar, State) :-
    get(initial_states, Grammar, [State]).

items_to_list(Items, ItemsList) :-
    assoc_to_list(Items, ItemsList).

empty_tables(Tables) :-
    empty_assoc(Tables).

create_tables(Grammar, Tables) :-
    get_counts(Grammar, Counts),
    get_properties(Grammar, Properties),
    length(Properties, PropertiesCount),
    items_to_list(Counts, CountsList),
    empty_tables(Tables0),
    CountsWithProperties = [property-PropertiesCount | CountsList],
    create_tables(Grammar, CountsWithProperties, Tables0, Tables).

create_tables(Grammar, [Name-Size | Rest], Tables0, Tables) :-
    number(Size),
    !,
    functor(Table, Name, Size),
    put_assoc(Name, Tables0, Table, Tables1),
    (   grammar:get(Name, Grammar, Items)
    ->  forall(
            member(Item, Items),
            (   item:get(index, Item, Index),
                Index1 is Index+1,
                (    item:get_entries(Item, Entries)
                ->   item:set_entries(Item, Entries, Item1)
                ;    Item1 = Item
                ),
            % if only using setarg, it will get out of local stack
                nb_linkarg(Index1, Table, Item1)
            )
              )
    ;   true
    ),
    create_tables(Grammar, Rest, Tables1, Tables).

create_tables(_Grammar, [], T, T).
