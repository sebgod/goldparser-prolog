:- module(shift_reduce_parser, [parser/2, parse_tokens/3, parse_token/4]).

:- use_module(grammar, []).
:- use_module(state,   []).
:- use_module(lalr,    []).
:- use_module(item,    []).
:- use_module(table,   []).
:- use_module(symbol,  []).
:- use_module(action,  []).
:- use_module(stack,   []).
:- use_module(table,   []).

:- debug, debug(parser).

parser(Grammar, parser(Grammar, Tables)) :-
    grammar:create_tables(Grammar, Tables).

%% parse(+Parser, +Tokens, ?Result) is det.
%
% Bottom-up shift-reduce parser.
%
parse_tokens(Parser, Tokens, ProgramN) :-
    reset(Parser, Program0),
    phrase(parse_tokens(Program0, ProgramN), Tokens, []).

reset(Parser, program(Parser, StateN, AST0)) :-
    Parser = parser(Grammar, _Tables),
    stack:empty(AST0),
    grammar:get_initial_states(Grammar, State0),
    state:merge(State0, [accept-false, action_count-0], StateN).

:- if(current_prolog_flag(debug, true)).
%% debug_parser_step(+P0, +PN, +ActionName, +Target) is det.
debug_parser_step(
    program(P0, State0, _AST0),
    program(P1, StateN, _AST1), ActionName, Target) :-
    (   ActionName == skip
    ->  Topic = parser(detail)
    ;   Topic = parser
    ),
    state:current(State0, lalr-Lalr0),
    state:current(StateN, lalr-LalrN),
    debug(Topic, '~p', parser_step(P0, P1,
                                   ActionName, Target, Lalr0-LalrN)).

debug_get_counter(State, Counter-Count) :-
    state:current(State, Counter-Count).

debug_increase_counter(
    program(Parser, State0, AST),
    Counter,
    program(Parser, StateN, AST)
                      ) :-
    debug_get_counter(State0, action_count-ActionCount),
    ActionCount1 is ActionCount + 1,
    state:merge(State0, [Counter-ActionCount1], StateN).

parse_tokens(Program, Program) -->
    {
     Program = program(_, State, _),
     debug_get_counter(State, action_count-ActionCount),
     (   ActionCount >= 20
     ->  !,
         debug(parser, 'action count >= ~w', ActionCount)
     )
    }.


:- else.
debug_parser_step(_, _, _, _).
debug_increase_counter(Program, _, Program).
:- endif.


parse_tokens(Program, Program, [], []) :- !.

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    parse_tokens(Program1, ProgramN).

parse_token(P0, PN) -->
    next_action(P0, ActionName, Target),
    perform(ActionName, Target, P0, P1),
    {
     debug_increase_counter(P1, action_count, PN),
     debug_parser_step(P0, PN, ActionName, Target)
    },
    !.

parse_token(Program, _, Tokens, Tokens) :-
    throw(error(representation_error('unexpected token'),
                context(parse_token//2, [Program, Tokens]))).

next_action(program(Parser, State, AST),
            ActionName, Target,
            Tokens, Tokens
           ) :-
    Parser = parser(_Grammar, Tables),
    lalr:current(Tables, State, Lalr),
    item:get_entries(Lalr, Actions),
    (   stack:peek(AST, SymbolIndex-_Data),
        ActionName = goto,
        symbol_to_action(Tables, SymbolIndex, Actions,
                         ActionName, Target)
    ->  !
    ;   lookahead(Tokens, SymbolIndex),
        symbol_to_action(Tables, SymbolIndex, Actions,
                         ActionName, Target)
    ).

symbol_to_action(Tables, SymbolIndex, Actions, ActionName, Target) :-
    symbol:by_type_name(Tables, SymbolTypeName, SymbolIndex, _Symbol),
    symbol_type_to_action(SymbolTypeName-SymbolIndex,
                          Actions, ActionName, Target).

lookahead(Tokens, SymbolIndex) :-
    [SymbolIndex-_Data | _] = Tokens.

symbol_type_to_action(noise-_SymbolIndex,
                      _Actions, skip, _Target) :- !.

symbol_type_to_action(accept-_SymbolIndex,
                      _Actions, accept, _Target) :- !.

symbol_type_to_action(SymbolType-SymbolIndex,
                      Actions, ActionName, Target) :-
    memberchk(SymbolType, [terminal, nonterminal]),
    action:find(Actions, SymbolIndex, FoundAction),
    item:get(action, FoundAction, ActionType),
    action:type(ActionType, ActionName),
    item:get(target, FoundAction, Target).

perform(skip, _, P, P, [_SkippedToken | TokenR], TokenR).

perform(shift, Target,
        program(parser(Grammar, Tables), State0, AST0),
        program(parser(Grammar, Tables), StateN, ASTN),
        [Token | TokenR], TokenR
       ) :-
    !,
    state:merge(State0, [lalr-Target], StateN),
    stack:push(AST0, Token, ASTN).

perform(reduce, Target,
        program(P, State0, AST0), program(P, State1, ASTN),
        Tokens, Tokens
       ) :-
    P = parser(_, Tables),
    table:item(rule_table, Tables, Target, Rule),
    item:get(head_index, Rule, HeadIndex),
    symbol:by_type_name(Tables, nonterminal, HeadIndex, Head),
    (   item:get_entries(Rule, RuleEntries)
    ->  RuleSymbols = RuleEntries
    ;   RuleSymbols = []
    ),
    item:entry_size(RuleSymbols, RuleSymbolSize),
    stack:rpop(AST0, RuleSymbolSize, Handles, AST1),
    update_reduction_state(P, AST1, State0, State1),
    item:get(name, Head, HeadName),
    Production =.. [HeadName | Handles],
    stack:push(AST1, HeadIndex-Production, ASTN).

perform(goto, Target,
        program(parser(Grammar, Tables), State0, AST),
        program(parser(Grammar, Tables), StateN, AST),
        Tokens, Tokens
       ) :-
    state:merge(State0, [lalr-Target], StateN).

perform(accept, _,
        program(parser(Grammar, Tables), State0, AST0),
        program(parser(Grammar, Tables), StateN, ASTN),
        Tokens, TokensR) :-
    (   [SymbolIndex-_ | TokensR] = Tokens,
        symbol:by_type_name(Tables, eof, SymbolIndex, _),
        TokensR = []
    ->  Accept = true
    ;   Accept = false
    ),
    state:merge(State0, [accept-Accept], StateN),
    ASTN = AST0.

update_reduction_state(_Parser, AST, State0, State1) :-
    % TODO: Reached the last production, so reset the LALR state
    % to the initial state (here 0), should be dynamic
    (   AST = []
    ->  state:merge(State0, [lalr-0], State1)
    ;   State1 = State0
    ).











