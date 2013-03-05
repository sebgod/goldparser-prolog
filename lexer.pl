:- module(lexer, [
                  scan_file/3,
                  scan_stream/3,
                  scan_list/3
                 ]).

:- use_module(support).
:- use_module(table, []).
:- use_module(state, []).
:- use_module(dfa, []).
:- use_module(symbol, []).

scan_file(Parser, Tokens, File) :-
    safe_open_file(File, scan_stream(Parser, Tokens), [encoding(utf8)]).

scan_stream(Parser, Tokens, Stream) :-
    stream_to_lazy_list(Stream, Input),
    scan_list(Parser, Tokens, Input).

scan_list(parser(G, T, State), Tokens, Input) :-
    state:merge(State, [last_accept-none, chars-''], Initial),
    scan_list_(parser(G, T, Initial), Tokens, Input).

scan_list_(parser(_G, Tables, _S), [Index-''], []) :-
    !,
    symbol:type(Kind, eof),
    once(symbol:by_type(Tables, Kind, Index, _)).

scan_list_(Parser, [ Token | TRest ], Input) :-
    phrase(eat_token(Parser, Token), Input, IRest),
    scan_list_(Parser, TRest, IRest).

eat_token(parser(G, Tables, State), Token) -->
    {
     !,
     dfa:current(Tables, State, DFA),
     state:current(State, chars-Chars0),
     state:current(State, last_accept-LastAccept)
    },
    (     [Input],
         { char_and_code(Input, Char, Code),
           dfa:find_edge(Tables, DFA, Code, TargetIndex)
         }
    ->     { table:item(dfa_table, Tables, TargetIndex, TargetDFA),
           dfa:accept(TargetDFA, Accept),
           atom_concat(Chars0, Char, Chars),
           state:merge(State,
                       [chars-Chars,
                        dfa-TargetIndex,
                        last_accept-Accept
                       ], NewState)
         },
         eat_token(parser(G, Tables, NewState), Token)
    ;    {LastAccept \= none,
          Token = LastAccept-Chars0,
          state:merge(State, [last_accept-none, chars-Char], NewState)
         }
    ).

