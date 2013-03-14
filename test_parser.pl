:- module(test_parser, [
                        test_scan_and_parse/1,
                        test_view/0
                       ]).

:- use_module(portray_grammar, []).
:- use_module(egt, []).
:- use_module(lexer, []).
:- use_module(shift_reduce_parser, []).
:- use_module(view_parser, []).

:- meta_predicate test_files(3, ?).

grammar(expression, 'ParserTest.egt').
grammar(gold, 'GOLD Meta-Language (2.6.0).egt').

grammar_test_files(expression, ['ParserTest.txt']).
%grammar_test_files(gold, ['GOLD Meta-Language (2.6.0).grm']).

test_scan_and_parse(Program) :-
    test_files(scan_and_parse, Program).

test_view :-
    test_files(view_graphs, _).

test_files(Tester, Program) :-
    grammar(GrammarName, GrammarFile),
    format('Grammar File: ~w~n', [GrammarFile]),
    load_parser(GrammarFile, Parser),
    grammar_test_files(GrammarName, TestFiles),
    member(TestFile, TestFiles),
    format('\tTesting: ~w~n~n', [TestFile]),
    call(Tester, Parser, TestFile, Program).

load_parser(File, Parser) :-
    egt:read_file(Grammar, File),
    shift_reduce_parser:parser(Grammar, Parser).

view_graphs(Parser, _TestFile, _) :-
    view_parser:view_parser(Parser).

scan_and_parse(Parser, TestFile, ProgramN) :-
    lexer:scan_file(Parser, TestFile, Tokens),
    shift_reduce_parser:parse_tokens(Parser, Tokens, ProgramN).











