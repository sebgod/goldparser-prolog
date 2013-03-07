:- module(shift_reduce_parser, [parser/2, parse_tokens/3, parse_token/4]).

:- use_module(state, []).
:- use_module(lalr, []).
:- use_module(item, []).
:- use_module(table, []).
:- use_module(symbol, []).
:- use_module(action, []).
:- use_module(stack, []).

:- use_module(library(assoc)).
:- use_module(table, []).

parser(Grammar, parser(Grammar, Tables)) :-
    Grammar = grammar(_Header, Assoc),
    create_tables(Assoc, Tables).

create_tables(Assoc, Tables) :-
    get_assoc(table_counts, Assoc, [Counts]),
    get_assoc(property, Assoc, Properties),
    length(Properties, PropertiesCount),
    assoc_to_list(Counts, CountsList),
    empty_assoc(E),
    CountsWithProperties = [property-PropertiesCount | CountsList],
    create_tables(Assoc, CountsWithProperties, E, Tables).

create_tables(Assoc, [Name-Size | Rest], Tables0, Tables) :-
    number(Size),
    !,
    functor(Table, Name, Size),
    put_assoc(Name, Tables0, Table, Tables1),
    (   get_assoc(Name, Assoc, Items)
    ->  forall(
            member(Item, Items),
            (   item:get(index, Item, Index),
                Index1 is Index+1,
                (    item:get_entries(Item, Entries)
                ->   item:set_entries(Item, Entries, Item1)
                ;    Item1 = Item
                ),
            % if only using setarg, it will get out of local stack
                nb_linkarg(Index1, Table, Item1)
            )
              )
    ;   true
    ),
    create_tables(Assoc, Rest, Tables1, Tables).

create_tables(_Assoc, [], T, T).

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
    % push the start symbol, stack:push(AST0, p(s), AST1),
    Grammar = grammar(_Header, Assoc),
    get_assoc(initial_states, Assoc, [State0]),
    state:merge(State0, [accept-false], StateN).

parse_tokens(Program, Program, [], []) :- !.

parse_tokens(Program0, ProgramN) -->
    parse_token(Program0, Program1),
    parse_tokens(Program1, ProgramN).

parse_token(P0, PN) -->
    next_action(P0, ActionName, Target),
    {format('~p~n', perform(ActionName, Target))},
    perform(ActionName, Target, P0, PN), !.

parse_token(Program, _, Tokens, Tokens) :-
    throw(error('Parsing token failed!',
                context(parse_token//2, [Program, Tokens])
               )
         ).

next_action(program(parser(_Grammar, Tables), State, AST),
            ActionName, Target,
            Tokens, Tokens
           ) :-
    lalr:current(Tables, State, Lalr),
    debug_parsing_state(Tokens, AST, Lalr, State),
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

debug_parsing_state(Tokens, AST, Lalr, State) :-
    state:current(State, lalr-LalrIndex),
    (   [Token | _] = Tokens
    ->  Lookahead = Token
    ;   Lookahead = none
    ),
    format('lalr-~d ast: ~p lookahead: ~p~n\tlalr: ~p~n',
           [LalrIndex, AST, Lookahead, Lalr]).


symbol_to_action(Tables, SymbolIndex, Actions, ActionName, Target) :-
    symbol:by_type_name(Tables, SymbolTypeName, SymbolIndex, _Symbol),
    format('trying symbol: ~w~n', [SymbolTypeName-SymbolIndex]),
    symbol_type_to_action(SymbolTypeName-SymbolIndex,
                          Actions, ActionName, Target).

lookahead(Tokens, SymbolIndex) :-
    [SymbolIndex-_Data | _] = Tokens.

symbol_type_to_action(noise-_SymbolIndex,
                      _Actions, skip, _Target) :- !.

symbol_type_to_action(accept-_SymbolIndex,
                      _Actions, accept, _Target) :- !.

symbol_type_to_action(_-SymbolIndex,
                      Actions, ActionName, Target) :-
    action:find(Actions, SymbolIndex, FoundAction),
    item:get(action, FoundAction, ActionType),
    action:type(ActionType, ActionName),
    item:get(target, FoundAction, Target),
    format('~w-> [~p] ~p~n~n', [ActionName, Target, FoundAction]).

perform(skip, _, P, P, [_SkippedToken | TokenR], TokenR).

perform(shift, Target,
        program(parser(Grammar, Tables), State0, AST0),
        program(parser(Grammar, Tables), StateN, ASTN),
        [Token | TokenR], TokenR
       ) :-
    !,
    state:merge(State0, [lalr-Target], StateN),
    stack:push(AST0, Token, ASTN),
    format('\t~p | ~p~n~n', [ASTN, TokenR]).

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
    format('\tarity: ~w ~p~n\tremain: ~p~n',
           [RuleSymbolSize, Handles, AST1]),
    Production =.. [p | Handles],
    stack:push(AST1, HeadIndex-Production, ASTN),
    format('\t~p | ~p~n\trule: ~p~n\thead: ~p~n~n',
           [ASTN, Tokens, Rule, Head]).

perform(goto, Target,
        program(parser(Grammar, Tables), State0, AST),
        program(parser(Grammar, Tables), StateN, AST),
        Tokens, Tokens
       ) :-
    state:merge(State0, [lalr-Target], StateN),
    table:item(lalr_table, Tables, Target, Lalr),
    format('\t~p | ~p~n\ttarget state: [~p] ~p~n~n', [AST, Tokens, Target, Lalr]).

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
    ASTN = AST0,
    format('accepted\t~p~n~n', [ASTN]).

update_reduction_state(_Parser, AST, State0, State1) :-
    % TODO: Reached the last production, so reset the LALR state
    % to the initial state (here 0), should be dynamic
    (   AST = []
    ->  state:merge(State0, [lalr-0], State1)
    ;   State1 = State0
    ).











