:- module(shift_reduce_parser, [parse_tokens/3, parse_token/4]).

:- use_module(lalr, []).
:- use_module(item, []).
:- use_module(symbol, []).
:- use_module(action, []).
:- use_module(ast, []).

%% parse(+Parser, +Tokens, ?Result) is det.
%
% Bottom-up shift-reduce parser.
%
parse_tokens(Parser, Tokens, Program) :-
    ast:empty(AST),
    phrase(parse_tokens(program(Parser, AST), Program), Tokens, []).

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    !,
    parse_tokens(Program1, ProgramN).

parse_tokens(Program, Program) --> [].

parse_token(P0, PN) -->
    next(P0, ActionName, Target),
    perform(ActionName, Target, P0, PN).

next(program(parser(_Grammar, Tables, State), _AST),
           ActionName, Target,
           Tokens, Tokens
          ) :-
    [SymbolIndex-Data | _] = Tokens,
    lalr:current(Tables, State, Lalr),
    symbol:by_type(Tables, SymbolType, SymbolIndex, Symbol),
    symbol:type(SymbolType, SymbolTypeName),
    (   SymbolTypeName = noise
    ->  ActionName = skip
    ;   item:get_entries(Lalr, Actions),
        action:find(Actions, SymbolIndex, Action),
        item:get(action, Action, ActionType),
        action:type(ActionType, ActionName),
        item:get(target, Action, Target)
    ),
    item:get(name, Symbol, SymbolName),
    format('a: ~p s: ~p a: ~p~n~n',
           [ActionName, SymbolTypeName-(SymbolName, Data), Action]).

perform(skip, _, P, P, [Token | TokenR], TokenR) :-
    writeln(Token).

perform(shift, Target,
            program(parser(Grammar, Tables, State0), AST0),
            program(parser(Grammar, Tables, StateN), ASTN),
            [Token | TokenR], TokenR
           ) :-
    !,
    state:merge(State0, [lalr-Target], StateN),
    ast:push(AST0, Token, ASTN).

perform(reduce, Target,
            program(P, AST0), program(P, ASTN),
            [Token | TokenR], TokenR
           ) :-
    ast:push(AST0, reduce-prod(Token, Target), ASTN).
