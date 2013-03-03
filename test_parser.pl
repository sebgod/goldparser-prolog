:- module(test_parser, [test/1, test/2, test/3]).

:- use_module(portray_grammar, []).
:- use_module(parse_grammar, []).
:- use_module(read_grammar, []).
:- use_module(lexer, []).
:- use_module(shift_reduce_parser, []).

test(AST) :- test(_, program(_, AST)).

test(Tokens, Program) :-
    test('ParserTest.egt', Tokens, Program).

test(File, Tokens, Program) :-
    format('File: ~w~n~n', [File]),
    read_grammar:read_file(Grammar, File),
    parse_grammar:parser(Grammar, Parser),
    lexer:scan_list(Parser, Tokens, "123 456\n"),
    shift_reduce_parser:parse_tokens(Parser, Tokens, Program).
