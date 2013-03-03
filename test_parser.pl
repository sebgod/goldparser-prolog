:- module(test_parser, [test/1, test/2, test/3]).

:- use_module(portray_grammar).
:- use_module(parse_grammar).
:- use_module(read_grammar).
:- use_module(lexer).
:- use_module(shift_reduce_parser).

test(AST) :- test(_, program(_, AST)).

test(Tokens, Program) :-
    test('ParserTest.egt', Tokens, Program).

test(File, Tokens, Program) :-
    format('File: ~w~n~n', [File]),
    read_grammar_file(Grammar, File),
    create_parser(Grammar, Parser),
    scan_list(Parser, Tokens, "123 456\n"),
    parse_tokens(Parser, Tokens, Program).
