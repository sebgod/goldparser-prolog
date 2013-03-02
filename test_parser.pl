:- module(test_parser, [test/2, test/3]).

:- use_module(portray_grammar).
:- use_module(parse_grammar).
:- use_module(read_grammar).
:- use_module(lexer).
:- use_module(shift_reduce_parser).

test(Parser, Tokens) :-
	test('ParserTest.egt', Tokens, Parser).

test(File, Tokens, Parser) :-
	format('File: ~w~n', [File]),
	read_grammar_file(Grammar, File),
	create_parser(Grammar, Parser),
	scan_list(Parser, Tokens, "123 456\n"),
	parse_tokens(Parser, Tokens, _Reductions).
