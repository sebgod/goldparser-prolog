:- module(lexer, [
                  scan_file/3,
                  scan_file_lazy/2,
                  scan_stream/3,
                  scan_stream_lazy/2,
                  scan_list/3,
                  scan_input//3,
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

scan_file_lazy(File, Input) :-
    safe_open_file(File, scan_stream_lazy(Input), [encoding(utf8)]).

scan_stream(Program, Tokens, Stream) :-
    stream_to_lazy_list(Stream, Input),
    scan_list(Program, Tokens, Input).

scan_stream_lazy(Input, Stream) :-
    stream_to_lazy_list(Stream, Input).

init(parser(Grammar, _Tables),
     lexer(dfa-DfaIndex, last_accept-none, chars-'')
    ) :-
    grammar:get_initial_states(Grammar, State),
    state:current(State, dfa-DfaIndex).

scan_list(Parser, Tokens, Input) :-
    init(Parser, Initial),
    scan_list_(Parser, Initial, Tokens, Input).

scan_list_(_, _, [], []) :- !.

scan_list_(Parser, Lexer, [ Token | TRest ], Input) :-
    phrase(scan_input(Parser, Lexer, Token), Input, InputR),
    scan_list_(Parser, Lexer, TRest, InputR).

scan_input(Parser, Lexer, Token) -->
    read_token(Parser, Lexer, Token),
    {
     !,
     debug_token_read(Parser, Token)
    }.

scan_input(parser(_G, Tables), _Lexer, Index-'') -->
    { once(symbol:by_type_name(Tables, eof, Index, _)) }.

debug_token_read(Parser, Token) :-
    debug(lexer, '~p', lexer_step(Parser, Token)).

try_restore_input([], [], []).
try_restore_input([Skipped | InputR], Skipped, InputR).


read_token(parser(G, Tables),
          lexer(dfa-DFAIndex, last_accept-LastAccept, chars-Chars0),
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
          NewState = lexer(dfa-TargetIndex,
                           last_accept-Accept,
                           chars-Chars)
        },
        read_token(parser(G, Tables), NewState, Token)
    ;   {
         (   LastAccept \= none
         ->  Token = LastAccept-Chars0
         ;   once(symbol:by_type_name(Tables, error, Index, _)),
             try_restore_input(Input, FailedInput, InputR),
             Input = [FailedInput | InputR],
             format(atom(Error), '~w', [FailedInput]),
             Token = Index-Error
         )
        }
    ).





