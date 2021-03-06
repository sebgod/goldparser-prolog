:- module(view_parser,
      [ view_parser/1, display/3 ]).

:- meta_predicate display_section(3,+,-,-).

:- pce_autoload(graph_viewer, graph_viewer).

:- use_module(library(pce)).
:- use_module(table, []).
:- use_module(item, []).
:- use_module(action, []).
:- use_module(symbol, []).

view_parser(Parser) :-
    Term = view_parser:display(Parser),
    new(GV, graph_viewer),
    send(GV, open),
    send(GV, status, full_screen),
    send(GV, generate, Term).

display(Parser, From, To) :-
    member(Kind, [display_lalr_states, display_rules]),
    display_section(Kind, Parser, From, To).

display_section(Kind, Parser, From, To) :-
    call(Kind, Parser, From, To).

iterate(Table, Tables, ItemIndex, Entry) :-
    table:items(Table, Tables, ItemIndex, Item),
    item:entries_nd(Item, Entry).

%display_dfa_states(parser(_G, Tables, _S), From, To) :-
%    iterate(dfa_table, Tables, DFAIndex, DFA, _).

display_lalr_states(parser(_G, Tables), From, To) :-
    iterate(lalr_table, Tables, LalrIndex, Action),
    item:value(action, Action, ActionType),
    action:type(ActionType, Name),
    display_lalr_action(Tables, LalrIndex, Name, Action, From, To).

display_rules(parser(_G, Tables), From, To) :-
    table:items(rule_table, Tables, _RuleIndex, Rule),
    display_production(Tables, Rule, From, To).

display_production(Tables, Rule, From, To) :-
    display_rule(From, Tables, Rule),
    item:entries_nd(Rule, Symbol),
    item:entry_value(symbol_index, Symbol, SymbolIndex),
    display_symbol(To, Tables, SymbolIndex).


display_lalr_action(Tables, LalrIndex, Name, Action, From, To) :-
    item:entry_value(target, Action, Target),
    item:entry_value(symbol_index, Action, SymbolIndex),
    (    format(atom(From), 'lalr-~d', [LalrIndex]),
         format(atom(To), '~w ~d -> ~d',
                [Name, LalrIndex, Target])
    ;    format(atom(From), '~w ~d -> ~d',
                [Name, LalrIndex, Target]),
         (    display_lalr_target(Tables, Name, To, Target)
         ;    (    Name = shift
              ->  display_symbol(To, Tables, SymbolIndex)
              )
         )
    ).

display_lalr_target(_Tables, Name, To, Target) :-
    memberchk(Name, [goto, shift]),
    format(atom(To), 'lalr-~d', [Target]).

display_lalr_target(Tables, reduce, To, Target) :-
    display_rule(To, Tables, Target).

display_lalr_target(Tables, accept, To, _Target) :-
    display_rule(To, Tables, 0).

display_rule(Atom, Tables, Index) :-
    number(Index), !,
    table:item('rule_table', Tables, Index, Rule),
    display_rule(Atom, Tables, Rule).

display_rule(Atom, Tables, Rule) :-
    item:value(head_index, Rule, HeadIndex),
    item:value(index, Rule, RuleIndex),
    display_symbol(Head, Tables, HeadIndex),
    format(atom(Atom), '~w-~d', [Head, RuleIndex]).

display_symbol(Atom, Tables, Index) :-
    number(Index), !,
    table:item('symbol_table', Tables, Index, Symbol),
    display_symbol(Atom, Tables, Symbol).

display_symbol(Atom, _Tables, Symbol) :-
    item:value(name, Symbol, Name),
    item:value(kind, Symbol, Kind),
    symbol:type(Kind, KindName, _),
    display_symbol_format(KindName, Format),
    format(atom(Atom), Format, [Name]).

display_symbol_format(nonterminal, '<~w>') :- !.
display_symbol_format(_, '~w').


