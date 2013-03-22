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
:- use_module(group,  []).
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
    lookahead(Lexer, Lookahead),
    scan_input(Lexer, Lookahead, Token, [], _GroupN),
    { debug_token_read(Lexer, Token) }.

%% scan_input(+Lexer, +Lookahead, -TokenN, +Groups0, -GroupsN)// is det.

scan_input(_Lexer, Lookahead, Token, [], []) -->
    { % no group there and no group start
      % -> eat the token
      Lookahead \= _-group_start(_)
    },
    !,
    (   { var(Token) } % we are not at the end of a group
    ->  advance(Lookahead),
        { Token = Lookahead }
    ;   { true }
    ).

scan_input(Lexer, Lookahead0, Token, Groups0, GroupsN) -->
    { Lookahead0 = SymbolIndex-group_start(GroupStart) },
    advance(Lookahead0),
    !,
    {
     Lexer = lexer(chars-_, dfa-_, last_accept-_, tables-Tables),
     group:by_symbol(Tables, start_index-SymbolIndex,
                     _GroupIndex, Group),
     item:get(container_index, Group, ContainerIndex),
     symbol:token(Tables, ContainerIndex, GroupStart, ContainerToken),
     (   (   Groups0 = []
         ;   Groups0 = [group(Top, _, _, _) | _],
             group:nestable(Top, SymbolIndex)
         )
     ->  item:get(advance_mode, Group, AdvanceIndex),
         item:get(ending_mode, Group, EndingIndex),
         group:advance_mode(AdvanceIndex, AdvanceMode),
         group:ending_mode(EndingIndex, EndingMode),
         GroupRec = group(Group, ContainerToken, AdvanceMode, EndingMode),
         Groups1 = [GroupRec | Groups0]
     ;   Groups1 = Groups0
     )
    },
    lookahead(Lexer, LookaheadN),
    scan_input(Lexer, LookaheadN, Token, Groups1, GroupsN).

scan_input(Lexer, Lookahead0, Token, Groups0, GroupsN) -->
    { Groups0 = [ group(Top, _, AdvanceMode, _) | _] },
    !,
    (   {
          Lookahead0 = SymbolIndex-_,
          item:get(end_index, Top, SymbolIndex)
        }
    ->  {  Groups0 = [group(_, Token, _, EndingMode) | Groups1]
        },
        (   { EndingMode = closed }
        ->  advance(Lookahead0)
        ;   { true }
        )
    ;   within_group(Lexer, AdvanceMode, Lookahead0, Groups0, Groups1)
    ),
    lookahead(Lexer, LookaheadN),
    scan_input(Lexer, LookaheadN, Token, Groups1, GroupsN).

within_group(_Lexer, character, _Lookahead, Groups0, GroupsN) -->
     [Input],
     !,
     { char_and_code(Input, Character, _Code),
       group:append_chars(Groups0, Character, GroupsN)
     }.

within_group(_Lexer, token, Lookahead, Groups0, GroupsN) -->
     advance(Lookahead),
     !,
     { group:append_chars(Groups0, Lookahead, GroupsN) }.

within_group(_, AdvanceMode, _, _, _) -->
    { throw(error('Unhandled advance mode',
                  context(analyze_lexical_group//4, AdvanceMode)
                 )
           )
    }.

advance(Token) -->
    { Token = _SymbolIndex-Data,
      (   Data == eof
      ->  Length = 0
      ;   arg(1, Data, TokenChars),
          atom_length(TokenChars, Length)
      )
    },
    list_skip(Length).

lookahead(Lexer, Token, InputChars, InputChars) :-
    read_token(Lexer, Token, InputChars).

read_token(lexer(chars-Chars0, dfa-DFAIndex,
                 last_accept-LastAccept, tables-Tables),
           Token, InputChars) :-
    (   InputChars = [Input | InputR],
        table:item(dfa_table, Tables, DFAIndex, DFA),
        char_and_code(Input, Char, Code),
        dfa:find_edge(Tables, DFA, Code, TargetIndex)
    ->  table:item(dfa_table, Tables, TargetIndex, TargetDFA),
        dfa:accept(TargetDFA, Accept),
        atom_concat(Chars0, Char, CharsN),
        NewState = lexer(chars-CharsN, dfa-TargetIndex,
                         last_accept-Accept, tables-Tables),
        read_token(NewState, Token, InputR)
    ;   (   LastAccept \= none
        ->  symbol:token(Tables, LastAccept, Chars0, Token)
        ;   (   ground(Input)
            ->  symbol:by_type_name(Tables, error, Index, _),
                Token = Index-error(Input)
            ;   symbol:by_type_name(Tables, eof, Index, _),
                Token = Index-eof
            )
        )
    ).

:- if(current_prolog_flag(debug, true)).
debug_token_read(Lexer, Token) :-
    debug(lexer, '~p', lexer_step('token read', Lexer, Token)).
:- else.
debug_token_read(_, _).
:-endif.
