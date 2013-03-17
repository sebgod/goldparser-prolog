:- module(shift_reduce_parser, [parser/2, init/2, parse_tokens/3]).

:- use_module(grammar, []).
:- use_module(state,   []).
:- use_module(item,    []).
:- use_module(table,   []).
:- use_module(lalr,    []).
:- use_module(symbol,  []).
:- use_module(action,  []).
:- use_module(stack,   []).
:- use_module(table,   []).

:- debug, debug(parser).

parser(Grammar, parser(Grammar, Tables)) :-
    grammar:tables(Grammar, Tables).

%% parse(+Parser, +Tokens, ?Result) is det.
%
% Bottom-up shift-reduce parser.
%
parse_tokens(Parser, Tokens, ProgramN) :-
    init(Parser, Program0),
    phrase(parse_tokens(Program0, ProgramN), Tokens, []).

init(Parser, program(Tables, state(accept-none), ASTN)) :-
    Parser = parser(Grammar, Tables),
    grammar:initial_states(Grammar, InitialState),
    state:current(InitialState, lalr-Lalr),
    stack:empty(AST0),
    stack:push(AST0, s(Lalr, start-''), ASTN).

:- if(current_prolog_flag(debug, true)).
%% debug_parser_step(+P0, +PN, +ActionName, +Target) is det.
debug_parser_step(
    program(P0, _State0, AST0),
    program(P1, _StateN, AST1), ActionName, Target) :-
    (   ActionName == skip
    ->  Target = LalrN, % skipping does not change the lalr state
        Topic = parser(detail)
    ;   Topic = parser
    ),
    lalr:index(AST0, Lalr0),
    lalr:index(AST1, LalrN),
    debug(Topic, '~p', parser_step(P0, P1,
                                   ActionName, Target, Lalr0-LalrN)).
:- else.
debug_parser_step(_, _, _, _).
:- endif.

parse_tokens(Program, Program, [], []) :- !.

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    parse_tokens(Program1, ProgramN).

parse_token(P0, PN) -->
    next_action(P0, ActionName, Target),
    perform(ActionName, Target, P0, PN),
    {
     debug_parser_step(P0, PN, ActionName, Target)
    },
    !.

parse_token(_P0, _P1, Tokens, Tokens) :-
    [SymbolIndex-Data | _] = Tokens,
    %symbol:type(SymbolType, SymbolTypeName),
    throw(error(representation_error('unexpected token'),
                context(parse_token//2, SymbolIndex-Data))).

next_action(program(Tables, _State, AST),
            ActionName, Target,
            Tokens, Tokens
           ) :-
    lalr:get(Tables, AST, Lalr),
    item:get_entries(Lalr, Actions),
    lookahead(Tokens, SymbolTypeName, SymbolIndex),
    symbol_to_action(SymbolTypeName, SymbolIndex,
                          Actions, ActionName, Target).

lookahead(Tokens, SymbolTypeName, SymbolIndex) :-
    [SymbolIndex-Data | _] = Tokens,
    functor(Data, SymbolTypeName, _).

symbol_to_action(noise, _SymbolIndex, _Actions, skip, _Target) :- !.

symbol_to_action(accept, _SymbolIndex, _Actions, accept, _Target) :- !.

symbol_to_action(SymbolType, SymbolIndex, Actions, ActionName, Target) :-
    memberchk(SymbolType, [terminal, eof, nonterminal]),
    action:find(Actions, SymbolIndex, FoundAction),
    item:get(action, FoundAction, ActionType),
    action:type(ActionType, ActionName),
    item:get(target, FoundAction, Target).

perform(skip, _Target, P, P, [_Skipped | TokenR], TokenR).

perform(shift, Target,
        program(Tables, State, AST0),
        program(Tables, State, ASTN),
        [Token | TokenR], TokenR
       ) :-
    !,
    stack:push(AST0, s(Target, Token), ASTN).

perform(reduce, Target,
        program(Tables, State, AST0),
        program(Tables, State, ASTN),
        Tokens, Tokens
       ) :-
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
    Reduction =.. [HeadName | Handles],
    update_reduction_state(Tables, HeadIndex-Reduction,  AST1, ASTN).

perform(accept, _Target,
        program(Tables, state(accept-none), AST0),
        program(Tables, state(accept-Accept), ASTN),
        Tokens, TokensR) :-
    (   [_-eof | TokensR] = Tokens,
        stack:pop(AST0, Reduction, AST1),
        stack:pop(AST1, s(InitLalr, start-_), AST2),
        stack:push(AST2, s(InitLalr, Reduction), ASTN),
        TokensR = []
    ->  Accept = true
    ;   Accept = false,
        ASTN = AST0
    ).

update_reduction_state(Tables, HeadIndex-Reduction, AST0, ASTN) :-
    lalr:get(Tables, AST0, LalrPrev),
    item:get_entries(LalrPrev, Actions),
    symbol_to_action(nonterminal, HeadIndex, Actions, goto, Goto),
    stack:push(AST0, s(Goto, HeadIndex-Reduction), ASTN).











