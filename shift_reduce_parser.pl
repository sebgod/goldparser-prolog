:- module(shift_reduce_parser, [parse_tokens/3, parse_token/3]).

:- use_module(lalr, []).
:- use_module(entries, []).
:- use_module(symbol, []).
:- use_module(action, []).
:- use_module(ast, []).

%% parse(+Parser, +Tokens, ?Result).
% Bottom-up shift-reduce parser
%
parse_tokens(Parser, Tokens, Program) :-
    ast:empty(AST),
	foldl(parse_token, Tokens, program(Parser, AST), Program).

parse_token(Token, P0, PN) :-
	Token = SymbolIndex-Data,
	P0 = program(parser(Grammar, Tables, State), AST),
	PN = program(parser(Grammar, Tables, NewState), NewAST),
	lalr:current(Tables, State, Lalr),
	symbol:by_type(Tables, SymbolType, SymbolIndex, Symbol),
	symbol:type(SymbolType, SymbolTypeName),
	format('~p [~p]: ~p ~n', [Symbol, SymbolTypeName, Data]),
	(	SymbolTypeName = noise
	->	NewState = State,
		NewAST = AST
	;	entries:list(Lalr, Actions),
		action:find(Actions, SymbolIndex, Action),
		format('action: ~p~n~n', [Action]),
		get_assoc(action, Action, ActionType),
		action:type(ActionType, ActionName),
		get_assoc(target, Action, Target),
		next_action(Token, Target, ActionName, P0, PN)
	).

next_action(Token, Target, shift,
			program(parser(Grammar, Tables, State0), AST0),
			program(parser(Grammar, Tables, StateN), ASTN)) :-
	!,
	state:merge(State0, [lalr-Target], StateN),
	ast:push(AST0, Token, ASTN).



next_action(Token, Target, ActionName,
			program(P, AST0),
			program(P, ASTN)) :-
	ast:push(AST0, ActionName-(Token, Target), ASTN).

%% parse(+S, ?Result) parses input string S,
% where Result is a list of categories to which it reduces.
parse(S, Result) :- shift_reduce(S,[],Result).

%% shift_reduce(+S,+Stack,?Result) parses input string S,
% where Stack is list of categories parsed so far

shift_reduce([],Result,Result) :- !.

shift_reduce(S,Stack,Result):-
	shift(Stack,S, NStack,S1), %fails if S = []
	reduce(NStack,ReducedStack),
	shift_reduce(S1,ReducedStack,Result).


	% shift(+Stack,+S,-NStack,-NS) shift 1st elem from S onto Stack.
	%
shift(X,[H|Y],[H|X],Y).

%% reduce(+Stack,ReducedStack) repeatedly reduce beginning of
% Stack to form fewer, larger constituents.
%
reduce(Stack,ReducedStack):-
	brule(Stack, Stack1),
	!,
	reduce(Stack1, ReducedStack).

reduce(Stack,Stack).

% Phrase structure rules (backward rules)
%
brule([c(vp), c(np) | X], [c(s) | X]).
brule([t(n), t(d) | X], [c(np) | X]).
brule([t(v) | X], [c(vp) | X]).
brule([c(np), t(v) | X], [c(vp) | X]).
brule([c(pp), c(np), t(v) | X], [c(vp) | X]).
brule([c(np), t(p) | X], [ c(pp) | X]).

brule([Noun | X], [t(n) | X]) :- n(Noun).
brule([Verb | X], [t(v) | X]) :- v(Verb).
brule([Prep | X], [t(p) | X]) :- p(Prep).

% Lexicon
%
n(dog).
n(dogs).
n(elephant).
n(elephants).
n(cat).
n(cats).
n(fish).
n(bird).
n(birds).
n(sheep).
n(garden).

v(like).
v(likes).
v(liked).
v(amuse).
v(amuses).
v(amused).
v(chase).
v(chases).
v(chased).
v(bark).
v(barks).
v(barked).
v(catch).
v(catches).
v(catched).

p(into).
