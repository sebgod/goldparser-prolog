:- module(portray_grammar, []).

user:portray(parser(Grammar, Tables, _)) :-
	Grammar = grammar(header(Header), Assoc),
	format('Version: ~w~n', [Header]),
	print_tables(Tables),
	assoc_to_keys(Assoc, AssocKeys),
	assoc_to_keys(Tables, TableKeys),
	ord_subtract(AssocKeys, TableKeys, Other),
	print_assoc_items(Assoc, Other).

user:portray('_entries'-Entries) :-
	Space = '        ',
	Entries =.. [entries | [First | Rest]],
	format('_entries: (~n~w~p', [Space, First]),

	forall(member(Entry, Rest), format(',~n~w~p', [Space, Entry])),
	format('~n~w)', [Space]).

user:portray(Assoc) :-
    assoc_to_list(Assoc, List),
	format('t~p', [List]).

print_tables(Tables) :-
	forall(gen_assoc(KTable, Tables, VTable),
		   (   VTable =.. [_ | VTableList],
			   print_list_vertical(KTable, VTableList)
		   )
		  ).

print_assoc_items(Assoc, List) :-
	forall(member(Item, List),
		   (   get_assoc(Item, Assoc, Values),
			   print_list_vertical(Item, Values)
		   )
		  ).

print_list_vertical(Name, List) :-
	length(List, Length),
	format('~w: ~d~n', [Name, Length]),
	forall(member(Value, List), format('    ~p~n', [Value])).

