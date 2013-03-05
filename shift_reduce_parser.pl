:- module(shift_reduce_parser, [parse_tokens/3, parse_token/4]).

:- use_module(state, []).
:- use_module(lalr, []).
:- use_module(item, []).
:- use_module(table, []).
:- use_module(symbol, []).
:- use_module(action, []).
:- use_module(stack, []).

%% parse(+Parser, +Tokens, ?Result) is det.
%
% Bottom-up shift-reduce parser.
%
parse_tokens(Parser, Tokens, Program) :-
    stack:empty(AST),
    phrase(parse_tokens(program(Parser, AST), Program), Tokens, []).

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    !,
    parse_tokens(Program1, ProgramN).

parse_tokens(Program, Program) --> [].

parse_token(P0, PN) -->
    next(P0, ActionName, Target),
    {format('~p~n', perform(ActionName, Target))},
    perform(ActionName, Target, P0, PN).

next(program(parser(_Grammar, Tables, State), AST),
     ActionName, Target,
     Tokens, Tokens
    ) :-
    state:current(State, lalr-LalrIndex),
    lalr:current(Tables, State, Lalr),
    format('peek [~p] current: ~p | ~p~n\tlalr: ~p~n~n',
           [LalrIndex, AST, Tokens, Lalr]),
    peek(AST, Tokens, SymbolIndex-_Data),
    symbol:by_type(Tables, SymbolType, SymbolIndex, _Symbol),
    symbol:type(SymbolType, SymbolTypeName),
    (   SymbolTypeName = noise
    ->  ActionName = skip
    ;   item:get_entries(Lalr, Actions),
        (   action:find(Actions, SymbolIndex, Action)
        ->  FoundAction = Action
        ;   action:list(Actions, [FoundAction])
        ),
        item:get(action, FoundAction, ActionType),
        action:type(ActionType, ActionName),
        item:get(target, FoundAction, Target),
        format('[~p] -> [~p] ~p~n~n',
               [LalrIndex, Target, FoundAction])
    ).

peek(_AST, Tokens, SymbolIndex-Data) :-
    [SymbolIndex-Data | _] = Tokens.
peek(AST, _Tokens, SymbolIndex-Data) :-
    stack:peek(AST, SymbolIndex-Data).

perform(skip, _, P, P, [_SkippedToken | TokenR], TokenR).

perform(shift, Target,
        program(parser(Grammar, Tables, State0), AST0),
        program(parser(Grammar, Tables, StateN), ASTN),
        [Token | TokenR], TokenR
       ) :-
    !,
    state:merge(State0, [lalr-Target], StateN),
    stack:push(AST0, Token, ASTN),
    format('shift\t~p | ~p~n~n', [ASTN, TokenR]).

perform(reduce, Target,
        program(P, AST0), program(P, ASTN),
        TokenR, TokenR
       ) :-
    P = parser(_, Tables, _),
    table:item(rule_table, Tables, Target, Rule),
    item:get(head_index, Rule, HeadIndex),
    symbol:by_type(Tables, nonterminal, HeadIndex, Head),
    (   item:get_entries(Rule, RuleEntries)
    ->  RuleSymbols = RuleEntries
    ;   RuleSymbols = []
    ),
    item:entry_size(RuleSymbols, RuleSymbolSize),
    functor(Production, p, RuleSymbolSize),
    stack:push(AST0, HeadIndex-Production, ASTN),
    format('reduce\t~p | ~p~n\trule: ~p~n\thead: ~p~n~n',
           [ASTN, TokenR, Rule, Head]),
    abort.

perform(goto, Target,
        program(parser(Grammar, Tables, State0), AST),
        program(parser(Grammar, Tables, StateN), AST),
        Tokens, Tokens
       ) :-
    state:merge(State0, [lalr-Target], StateN),
    table:item(lalr_table, Tables, Target, Lalr),
    format('goto\t~p | ~p~n\ttarget state: [~p] ~p~n~n', [AST, Tokens, Target, Lalr]).

perform(accept, _,
        program(parser(Grammar, Tables, State0), AST0),
        program(parser(Grammar, Tables, StateN), ASTN),
        Tokens, Tokens) :-
    state:merge(State0, [accept-true], StateN),
    ASTN = AST0,
    format('accepted\t~p~n~n', [ASTN]).
