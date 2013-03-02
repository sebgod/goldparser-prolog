:- module(dfa, [
	current/3,
	accept/2,
	find_edge/4
	]).

:- use_module(entries).
:- use_module(table).
:- use_module(state).
:- use_module(charset).

current(Tables, State, DFA) :-
	state:current_item(Tables, State, dfa-dfa_table, DFA).

accept(DFA, Accept) :-
	(    get_assoc(accept_state, DFA, true)
	->   get_assoc(accept_index, DFA, Accept)
	;    Accept = none
	).

find_edge(Tables, DFA, Code, TargetIndex) :-
	entries:list(DFA, Entries),
	(	entries:iterate(Entries, Entry, _EntryIndex),
		get_assoc(target_index, Entry, TargetIndex),
		get_assoc(character_set_index, Entry, CharsetIndex),
		table:item(character_set_table, Tables, CharsetIndex, Charset),
		charset:member(Charset, Code) -> !
	).
