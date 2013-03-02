:- module(parse_grammar, [create_parser/2]).

:- use_module(entries, []).

create_parser(Grammar, parser(Grammar, Tables, State)) :-
	Grammar = grammar(_Header, Assoc),
	create_tables(Assoc, Tables),
	get_assoc(initial_states, Assoc, [State]).

create_tables(Assoc, Tables) :-
	get_assoc(table_counts, Assoc, [Counts]),
	get_assoc(property, Assoc, Properties),
	length(Properties, PropertiesCount),
	assoc_to_list(Counts, CountsList),
	empty_assoc(E),
	CountsWithProperties = [property-PropertiesCount | CountsList],
	create_tables(Assoc, CountsWithProperties, E, Tables).

create_tables(Assoc, [Name-Size | Rest], Tables0, Tables) :-
	number(Size),
	!,
	functor(Table, Name, Size),
	put_assoc(Name, Tables0, Table, Tables1),
	(   get_assoc(Name, Assoc, Items)
	->  forall(
			member(Item, Items),
			(   get_assoc(index, Item, Index),
				Index1 is Index+1,
				(	entries:list(Item, Entries)
				->	EntriesTerm =.. [entries | Entries],
					put_assoc('_entries', Item, EntriesTerm, Item1)
				;	Item1 = Item
				),
			% if only using setarg, it will get out of local stack
				nb_linkarg(Index1, Table, Item1)
			)
			  )
	;   true
	),
	create_tables(Assoc, Rest, Tables1, Tables).

create_tables(_Assoc, [], T, T).










