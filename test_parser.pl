:- module(test_parser, [
                        test_scan/1,
                        test_view/1,
                        test_view/2,
                        test_view/3,
                        load_parser/2,
                        scan_and_parse/2
                       ]).

:- use_module(portray_grammar, []).
:- use_module(parse_grammar, []).
:- use_module(read_grammar, []).
:- use_module(lexer, []).
:- use_module(shift_reduce_parser, []).
:- use_module(view_parser, []).

% these are only used in the test grammar
user:portray(symbol_index-0) :- write(symbol_index-'0#(EOF)').
user:portray(symbol_index-1) :- write(symbol_index-'1#(ERROR)').
user:portray(symbol_index-2) :- write(symbol_index-'2#Whitespace').
user:portray(symbol_index-3) :- write(symbol_index-'3#IntegerLiteral').
user:portray(symbol_index-4) :- write(symbol_index-'4#<Program>').

test_scan(Program) :-
    test_scan('ParserTest.egt', Program).

test_scan(File, Program) :-
    format('File: ~w~n~n', [File]),
    load_parser(File, Parser),
    scan_and_parse(Parser, Program).

load_parser(File, Parser) :-
    read_grammar:read_file(Grammar, File),
    parse_grammar:parser(Grammar, Parser).

test_view(AST) :- test_view(_, program(_, AST)).

test_view(Tokens, Program) :-
    test_view('ParserTest.egt', Tokens, Program).

test_view(File, _Tokens, _Program) :-
    format('File: ~w~n~n', [File]),
    load_parser(File, Parser),
    view_parser:view_parser(Parser).

scan_and_parse(Parser, Program) :-
    lexer:scan_list(Parser, Tokens, "1234"),
    shift_reduce_parser:parse_tokens(Parser, Tokens, Program).
