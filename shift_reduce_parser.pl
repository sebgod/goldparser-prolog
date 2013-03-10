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

actions(Tables, State, Actions) :-
    lalr:current(Tables, State, Lalr),
    item:get_entries(Lalr, Actions).

reset(Parser, program(Parser, StateN, AST0)) :-
    Parser = parser(Grammar,Tables),
    stack:empty(AST0),
    grammar:get_initial_states(Grammar, State0),
    actions(Tables, State0, Actions),
    action:init_gotos(Actions, Gotos),
    state:merge(State0, [accept-false, step_count-0, goto-Gotos], StateN).

:- if(current_prolog_flag(debug, true)).
%% debug_parser_step(+P0, +PN, +ActionName, +Actions, +Target) is det.
debug_parser_step(
    program(P0, State0, _AST0),
    program(P1, StateN, _AST1), ActionName, Actions, Target) :-
    (   ActionName == skip
    ->  Topic = parser(detail)
    ;   Topic = parser
    ),
    state:current(State0, lalr-Lalr0),
    state:current(StateN, lalr-LalrN),
    forall(item:entry_member(Actions, Action, _),
           debug(parser, '~p', Action)
          ),
    debug(Topic, '~p', parser_step(P0, P1,
                                   ActionName, Target, Lalr0-LalrN)).

debug_get_counter(State, Counter-Count) :-
    state:current(State, Counter-Count).

debug_increase_counter(
    program(Parser, State0, AST),
    Counter,
    program(Parser, StateN, AST)
                      ) :-
    debug_get_counter(State0, step_count-ActionCount),
    ActionCount1 is ActionCount + 1,
    state:merge(State0, [Counter-ActionCount1], StateN).

parse_tokens(Program, Program) -->
    {
     Program = program(_, State, _),
     debug_get_counter(State, step_count-StepCount),
     (   StepCount >= 100
     ->  !,
         debug(parser, 'action count >= ~w', StepCount)
     )
    }.


:- else.
debug_parser_step(_, _, _, _, _).
debug_increase_counter(Program, _, Program).
:- endif.


parse_tokens(Program, Program, [], []) :- !.

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    parse_tokens(Program1, ProgramN).

parse_token(P0, PN) -->
    next_action(P0, ActionName, Actions, Target),
    perform(ActionName, Actions, Target, P0, P1),
    {
     debug_increase_counter(P1, step_count, PN),
     debug_parser_step(P0, PN, ActionName, Actions, Target)
    },
    !.

parse_token(_P0, _P1, Tokens, Tokens) :-
    throw(error(representation_error('unexpected token'),
                context(parse_token//2, Tokens))).

next_action(program(Parser, State, _AST),
            ActionName, Actions, Target,
            Tokens, Tokens
           ) :-
    Parser = parser(_Grammar, Tables),
    actions(Tables, State, Actions),
    lookahead(Tokens, SymbolIndex),
    symbol_to_action(Tables, SymbolIndex, Actions,
                     ActionName, Target).

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
    memberchk(SymbolType, [terminal, eof, nonterminal]),
    action:find(Actions, SymbolIndex, FoundAction),
    item:get(action, FoundAction, ActionType),
    action:type(ActionType, ActionName),
    item:get(target, FoundAction, Target).

perform(skip, _Actions, _Target, P, P, [_Skipped | TokenR], TokenR).

perform(shift, Actions, Target,
        program(parser(Grammar, Tables), State0, AST0),
        program(parser(Grammar, Tables), StateN, ASTN),
        [Token | TokenR], TokenR
       ) :-
    !,
    state:current(State0, goto-Goto0),
    action:update_gotos(Actions, Goto0, Goto1),
    state:merge(State0, [lalr-Target, goto-Goto1], StateN),
    stack:push(AST0, Token, ASTN).

perform(reduce, _Actions, Target,
        program(Parser, State0, AST0),
        program(Parser, State1, ASTN),
        Tokens, Tokens
       ) :-
    Parser = parser(_, Tables),
    table:item(rule_table, Tables, Target, Rule),
    item:get(head_index, Rule, HeadIndex),
    symbol:by_type_name(Tables, nonterminal, HeadIndex, Head),
    (   item:get_entries(Rule, RuleEntries)
    ->  RuleSymbols = RuleEntries
    ;   RuleSymbols = []
    ),
    item:entry_size(RuleSymbols, RuleSymbolSize),
    stack:rpop(AST0, RuleSymbolSize, Handles, AST1),
    item:get(name, Head, HeadName),
    Production =.. [HeadName | Handles],
    stack:push(AST1, HeadIndex-Production, ASTN),
    update_reduction_state(ASTN, State0, State1).

perform(goto, _Actions, Target,
        program(parser(Grammar, Tables), State0, AST),
        program(parser(Grammar, Tables), StateN, AST),
        Tokens, Tokens
       ) :-
    state:merge(State0, [lalr-Target], StateN).

perform(accept, _Actions, _Target,
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

update_reduction_state(ASTN, State0, State1) :-
    (   state:current(State0, goto-Gotos),
        stack:peek(ASTN, SymbolIndex-_Data),
        item:get(SymbolIndex, Gotos, Goto)
    ->  TargetLalr = Goto
    ;   state:current(State0, goto-Gotos),
        debug(parser, '~p', goto(Gotos)),
        state:current(State0, lalr-TargetLalr)
    ),
    state:merge(State0, [lalr-TargetLalr], State1).











