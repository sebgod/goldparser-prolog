:- module(read_grammar, [
						 read_file/2,
						 read_stream/2
						]).

:- use_module(read_multitype).
:- use_module(read_primitive).
:- use_module(library(assoc)).
:- use_module(support).

read_file(Grammar, File) :-
	safe_open_file(File, read_stream(Grammar)).

read_stream(grammar(Header, Assoc), Stream) :-
	read_header(Stream, Header),
	empty_assoc(Empty),
	read_structures(Stream, Empty, Assoc).

read_structures(Stream) -->
	read_structure(Stream),
	!,
	read_structures(Stream).
read_structures(_, S, S).

read_header(Stream, header(Header)) :- read_utf16le_z(Stream, Header).

read_structure(Stream, PrevAssoc, NewAssoc) :-
	read_ascii_char(Stream, Type),
	read_record(Stream, Type, Record),
	sort_record(Record, Key, Value),
	assoc_append_list(PrevAssoc, Key, Value, NewAssoc).







