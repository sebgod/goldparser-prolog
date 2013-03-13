:- module(lexer, [
                  scan_file/3,
                  scan_stream/3,
                  scan_list/3
                 ]).

:- use_module(support).
:- use_module(table,  []).
:- use_module(state,  []).
:- use_module(dfa,    []).
:- use_module(symbol, []).

scan_file(Program, Tokens, File) :-
    safe_open_file(File, scan_stream(Program, Tokens), [encoding(utf8)]).

scan_stream(Program, Tokens, Stream) :-
    stream_to_lazy_list(Stream, Input),
    scan_list(Program, Tokens, Input).

scan_list(program(parser(G, T), State, _AST), Tokens, Input) :-
    state:merge(State, [last_accept-none, chars-''], Initial),
    scan_list_(parser(G, T), Initial, Tokens, Input).

scan_list_(parser(_G, Tables), _State, [Index-''], []) :-
    !,
    once(symbol:by_type_name(Tables, eof, Index, _)).

scan_list_(Parser, State, [ Token | TRest ], Input) :-
    (   phrase(eat_token(Parser, State, Token), Input, InputR)
    ->  !
    ;   Parser = parser(_G, Tables),
        once(symbol:by_type_name(Tables, error, Index, _)),
        try_restore_input(Input, FailedInput, InputR),
        Input = [FailedInput | InputR],
        format(atom(Error), '~w', [FailedInput]),
        Token = Index-Error
    ),
    debug_token_read(Parser, Token),
    scan_list_(Parser, State, TRest, InputR).

debug_token_read(Parser, Token) :-
    debug(lexer, '~p', lexer_step(Parser, Token)).

try_restore_input([], [], []).
try_restore_input([Skipped | InputR], Skipped, InputR).

eat_token(parser(G, Tables), State, Token) -->
    {
     !,
     dfa:current(Tables, State, DFA),
     state:current(State, chars-Chars0),
     state:current(State, last_accept-LastAccept)
    },
    (   [Input],
        { char_and_code(Input, Char, Code),
          dfa:find_edge(Tables, DFA, Code, TargetIndex)
        }
    ->  { table:item(dfa_table, Tables, TargetIndex, TargetDFA),
          dfa:accept(TargetDFA, Accept),
          atom_concat(Chars0, Char, Chars),
          state:merge(State,
                      [chars-Chars,
                       dfa-TargetIndex,
                       last_accept-Accept
                      ], NewState)
        },
        eat_token(parser(G, Tables), NewState, Token)
    ;   {LastAccept \= none,
         Token = LastAccept-Chars0,
         state:merge(State, [last_accept-none, chars-Char], NewState)
        }
    ).

