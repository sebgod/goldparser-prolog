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
     lexer(chars-'', dfa-DfaIndex, last_accept-none, tables-Tables)
    ) :-
    grammar:initial_states(Grammar, State),
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
    lookahead(Lexer, Token0),
    analyze_lexical_group(Lexer, Token0, TokenN, Groups0, GroupsN),
    { debug_token_read(Lexer, TokenN) }.

analyze_lexical_group(_Lexer, Token, Token, Groups0, GroupsN) -->
    {
     Token = _Index-group_start(_),
     stack:push(Groups0, group(Token), GroupsN)
    }.

analyze_lexical_group(_Lexer,
                      Token, Token,
                      Groups, Groups) -->
    { stack:empty(Groups),  % no group there, eat the token
      Token = _SymbolIndex-Data,
      (   Data == eof
      ->  Length = 0
      ;   arg(1, Data, TokenChars),
          atom_length(TokenChars, Length)
      )
    },
    list_skip(Length).


lookahead(Lexer, Token, InputChars, InputChars) :-
    read_token(Lexer, Token, InputChars, _).

read_token(lexer(chars-Chars0, dfa-DFAIndex,
                 last_accept-LastAccept, tables-Tables),
           Token) -->
    (   [Input],
        {
         dfa:current(Tables, DFAIndex, DFA),
         char_and_code(Input, Char, Code),
         dfa:find_edge(Tables, DFA, Code, TargetIndex)
        }
    ->  {
         table:item(dfa_table, Tables, TargetIndex, TargetDFA),
         dfa:accept(TargetDFA, Accept),
         atom_concat(Chars0, Char, CharsN),
         NewState = lexer(chars-CharsN, dfa-TargetIndex,
                          last_accept-Accept, tables-Tables)
        },
        read_token(NewState, Token)
    ;   {
         (   LastAccept \= none
         ->  symbol:by_type_name(Tables, Type, LastAccept, _Symbol),
             Data =.. [Type, Chars0],
             Token = LastAccept-Data
         ;   (   ground(Input)
             ->  symbol:by_type_name(Tables, error, Index, _),
                 Token = Index-error(Input)
             ;   symbol:by_type_name(Tables, eof, Index, _),
                 Token = Index-eof
             )
         )
        }
    ).

:- if(current_prolog_flag(debug, true)).
debug_token_read(Lexer, Token) :-
    debug(lexer, '~p', lexer_step('token read', Lexer, Token)).
:- else.
debug_token_read(_, _).
:-endif.
