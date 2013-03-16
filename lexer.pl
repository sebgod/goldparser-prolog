:- module(lexer, [
                  scan_file/3,
                  scan_stream/3,
                  scan_list/3,
                  scan_input//2,
                  init/2
                 ]).

:- use_module(support).
:- use_module(grammar,[]).
:- use_module(table,  []).
:- use_module(state,  []).
:- use_module(dfa,    []).
:- use_module(symbol, []).

scan_file(Parser, File, Tokens) :-
    safe_open_file(File, scan_stream(Parser, Tokens), [encoding(utf8)]).

scan_stream(Program, Tokens, Stream) :-
    stream_to_lazy_list(Stream, Input),
    scan_list(Program, Tokens, Input).

init(parser(Grammar, Tables),
     lexer(tables-Tables, dfa-DfaIndex, last_accept-none, chars-'',
           group_marker-_GroupChars)
    ) :-
    grammar:get_initial_states(Grammar, State),
    state:current(State, dfa-DfaIndex).

scan_list(Parser, Tokens, Input) :-
    init(Parser, Lexer),
    scan_list_(Lexer, Tokens, Input).

scan_list_(_, [], []) :- !.

scan_list_(Lexer, [ Token | TRest ], Input) :-
    phrase(scan_input(Lexer, Token), Input, InputR),
    scan_list_(Lexer, TRest, InputR).

scan_input(Lexer, Token) -->
    { stack:empty(Group0) },
    scan_input(Lexer, Token, Group0, _GroupN).

scan_input(Lexer, TokenN, Groups0, GroupsN) -->
    read_token(Lexer, Token0),
    analyze_lexical_group(Lexer, Token0, TokenN, Groups0, GroupsN),
    { debug_token_read(Lexer, TokenN) }.

analyze_lexical_group(_Lexer, Token, Token, Groups0, GroupsN) -->
    {
     stack:push(Groups0, g(Token), GroupsN)
    }.

analyze_lexical_group(_Lexer,
                      Token, Token,
                      Groups, Groups,
                      Input, Input).

debug_token_read(Lexer, Token) :-
    debug(lexer, '~p', lexer_step(Lexer, Token)).

try_restore_input([], [], []).
try_restore_input([Skipped | InputR], Skipped, InputR).

read_token(lexer(tables-Tables, dfa-DFAIndex, last_accept-LastAccept,
                 chars-Chars0, group_marker-GroupMarker),
           Token) -->
    (   [Input],
        {
         dfa:current(Tables, DFAIndex, DFA),
         char_and_code(Input, Char, Code),
         dfa:find_edge(Tables, DFA, Code, TargetIndex)
        }
    ->  { table:item(dfa_table, Tables, TargetIndex, TargetDFA),
          dfa:accept(TargetDFA, Accept),
          atom_concat(Chars0, Char, Chars),
          NewState = lexer(tables-Tables, dfa-TargetIndex,
                           last_accept-Accept,
                           chars-Chars, group_marker-GroupMarker)
        },
        read_token(NewState, Token)
    ;   {
         (   LastAccept \= none
         ->  Token = LastAccept-Chars0
         ;   (   ground(Input)
             ->  once(symbol:by_type_name(Tables, error, Index, _)),
                 try_restore_input(Input, FailedInput, InputR),
                 Input = [FailedInput | InputR],
                 format(atom(Error), '~w', [FailedInput]),
                 Token = Index-Error
             ;   once(symbol:by_type_name(Tables, eof, Index, _)),
                 Token = Index-''
             )
         )
        }
    ).
