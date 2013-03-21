:- module(test_parser, [
                        test_scan_and_parse/1,
                        test_view/0
                       ]).
:- debug(lexer), debug(parser), debug(parser(detail)).

:- use_module(support).
:- use_module(portray_grammar, []).
:- use_module(egt, []).
:- use_module(lexer, []).
:- use_module(state, []).
:- use_module(shift_reduce_parser, []).
:- use_module(view_parser, []).

:- meta_predicate test_files(3, ?).

grammar(gold, 'GOLD Meta-Language (2.6.0).egt').
grammar(expression, 'ParserTest.egt').

grammar_test_files(expression, ['ParserTest.txt']).
grammar_test_files(gold, ['GOLD Meta-Language (2.6.0).grm']).

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
    format('\tTest: ~w~n~n', [TestFile]),
    call(Tester, Parser, TestFile, Program).

load_parser(File, Parser) :-
    egt:read_file(Grammar, File),
    shift_reduce_parser:parser(Grammar, Parser).

view_graphs(Parser, _TestFile, _) :-
    view_parser:view_parser(Parser).

scan_and_parse(Parser, TestFile, ProgramN) :-
    lexer:init(Parser, Lexer),
    shift_reduce_parser:init(Parser, Program0),
    safe_open_file(TestFile,
                   scan_and_parse_stream(Program0, Lexer, ProgramN),
                   [encoding(utf8)]).

scan_and_parse_stream(Program0, Lexer, ProgramN, Stream) :-
    stream_to_lazy_list(Stream, Input),
    phrase(scan_and_parse_dcg(Program0, Lexer, ProgramN),
           Input, []).

scan_and_parse_dcg(Program, _, Program) -->
    [],
    {
     Program = program(_, state(accept-Accept), _),
     Accept \= none
    },
    !.

scan_and_parse_dcg(Program0, Lexer, ProgramN) -->
    lexer:scan_input(Lexer, Token),
    {
      phrase(shift_reduce_parser:parse_tokens(Program0, Program1),
             [Token], [])
    },
    scan_and_parse_dcg(Program1, Lexer, ProgramN).

%scan_and_parse_dcg(Program, _, Program, [], []) :- !.
