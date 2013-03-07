:- module(test_parser, [
                        test_scan/1,
                        test_view/3
                       ]).

:- use_module(portray_grammar, []).
:- use_module(egt, []).
:- use_module(lexer, []).
:- use_module(shift_reduce_parser, []).
:- use_module(view_parser, []).

test_data('ParserTest.egt', ['ParserTest.txt']).
test_data('GOLD Meta-Language (2.6.0).egt', ['GOLD Meta-Language (2.6.0).grm']).

test_scan(Program) :-
    test_data(GrammarFile, TestFiles),
    test_scan(GrammarFile, TestFiles, Program).

test_scan(GrammarFile, TestFiles, Program) :-
    format('Grammar File: ~w~n~n', [GrammarFile]),
    load_parser(GrammarFile, Parser),
    member(TestFile, TestFiles),
    scan_and_parse(Parser, TestFile, Program).

load_parser(File, Parser) :-
    egt:read_file(Grammar, File),
    shift_reduce_parser:parser(Grammar, Parser).

test_view(File, _Tokens, _Program) :-
    format('File: ~w~n~n', [File]),
    test_data(File, _TestFiles),
    load_parser(File, Parser),
    view_parser:view_parser(Parser).

scan_and_parse(Parser, File, ProgramN) :-
    shift_reduce_parser:reset(Parser, Program0),
    lexer:scan_file(Program0, Tokens, File),
    shift_reduce_parser:parse_tokens(Parser, Tokens, ProgramN).











