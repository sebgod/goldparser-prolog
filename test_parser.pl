:- module(test_parser, [test/1, test/2, test/3]).

:- use_module(portray_grammar, []).
:- use_module(parse_grammar, []).
:- use_module(read_grammar, []).
:- use_module(lexer, []).
:- use_module(shift_reduce_parser, []).

% these are only used in the test grammar
user:portray(symbol_index-0) :- write(symbol_index-'0#(EOF)').
user:portray(symbol_index-1) :- write(symbol_index-'1#(ERROR)').
user:portray(symbol_index-2) :- write(symbol_index-'2#Whitespace').
user:portray(symbol_index-3) :- write(symbol_index-'3#IntegerLiteral').
user:portray(symbol_index-4) :- write(symbol_index-'4#<Program>').


test(AST) :- test(_, program(_, AST)).

test(Tokens, Program) :-
    test('ParserTest.egt', Tokens, Program).

test(File, Tokens, Program) :-
    format('File: ~w~n~n', [File]),
    read_grammar:read_file(Grammar, File),
    parse_grammar:parser(Grammar, Parser),
    lexer:scan_list(Parser, Tokens, "1"),
    shift_reduce_parser:parse_tokens(Parser, Tokens, Program).
