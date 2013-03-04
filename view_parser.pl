
:- module(view_parser,
	  [ view_parser/1
	  ]).

:- use_module(library(pce)).
:- use_module(table, []).
:- use_module(item, []).
:- use_module(action, []).
:- use_module(symbol, []).
:- require([ forall/2
	   , term_variables/2
	   , random/3
	   , term_to_atom/2
	   ]).

view_parser(Parser) :-
	Parser = parser(grammar(_Header, _), _, _),
	new(GV, graph_viewer),
	send(GV, open),
	send(GV, status, full_screen),
	send(GV, generate, Parser).

display(Parser, From, To) :-
	member(Kind, [display_lalr_states, display_rules]),
	call(Kind, Parser, From, To).

display_lalr_states(parser(_G, Tables, _S), From, To) :-
	table:items(lalr_table, Tables, LalrIndex, Lalr),
	item:get_entries(Lalr, Actions),
    item:entries(Actions, Action, _ActionIndex),
	item:get(action, Action, ActionType),
	action:type(ActionType, Name),
	display_lalr_action(Tables, LalrIndex, Name, Action, From, To).

display_rules(parser(_G, Tables, _S), From, To) :-
	table:items(rule_table, Tables, _RuleIndex, Rule),
    display_production(Tables, Rule, From, To).

display_production(Tables, Rule, From, To) :-
	display_rule(From, Tables, Rule),
	item:get_entries(Rule, Symbols),
	item:entries(Symbols, Symbol, _),
    item:get(symbol, Symbol, SymbolIndex),
    display_symbol(To, Tables, SymbolIndex).


display_lalr_action(Tables, LalrIndex, Name, Action, From, To) :-
	item:get(target, Action, Target),
	item:get(symbol_index, Action, SymbolIndex),
	(	format(atom(From), 'lalr-~d', [LalrIndex]),
		format(atom(To), '~w ~d -> ~d',
			   [Name, LalrIndex, Target])
	;	format(atom(From), '~w ~d -> ~d',
			   [Name, LalrIndex, Target]),
		(	display_lalr_target(Tables, Name, To, Target)
		;	(	Name = shift
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
	item:get(head_index, Rule, HeadIndex),
	item:get(index, Rule, RuleIndex),
	display_symbol(Head, Tables, HeadIndex),
	format(atom(Atom), '~w-~d', [Head, RuleIndex]).

display_symbol(Atom, Tables, Index) :-
	number(Index), !,
	table:item('symbol_table', Tables, Index, Symbol),
	display_symbol(Atom, Tables, Symbol).

display_symbol(Atom, _Tables, Symbol) :-
	item:get(name, Symbol, Name),
	item:get(kind, Symbol, Kind),
	symbol:type(Kind, KindName),
	display_symbol_format(KindName, Format),
	format(atom(Atom), Format, [Name]).

display_symbol_format(nonterminal, '<~w>') :- !.
display_symbol_format(_, '~w').


:- pce_begin_class(graph_viewer, frame).


initialise(GV) :->
	"Create graph-viewer"::
	send(GV, send_super, initialise, 'GOLD Parser Viewer'),
	send(GV, append, new(P, picture)),
	send(new(D, dialog), below, P),
	fill_dialog(D).

fill_dialog(D) :-
	new(Frame, D?frame),
	send(D, append, label(reporter)),
	send(D, append, button(quit, message(Frame, destroy))),
	%send(D, append, button(clear, message(Frame, clear))),
	send(D, append, button(postscript, message(Frame, postscript))),
	send(D, append, button(layout, message(Frame, layout))).


clear(F) :->
	"Clear the diagram"::
	get(F, member, picture, P),
	send(P, clear).


layout(F) :->
	"Run graph layout"::
	get(F, member, picture, P),
	(   get(P?graphicals, head, Head)
	->  send(Head, layout)
	;   send(F, report, error, 'No graph to layout')
	).


:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

postscript(F) :->
	"Create PostScript in file"::
	get(@finder, file, @off, '.eps', FileName),
	get(F, member, picture, Pict),
	new(File, file(FileName)),
	send(File, open, write),
	send(File, append, Pict?postscript),
	send(File, close),
	send(File, done),
	send(F, report, status, 'Saved PostScript in %s', FileName).

generate(F, Parser:term) :->
	"Create graph using generator"::
	send(F, clear),
	forall(display(Parser, From, To),
		   send(F, display_arc, From, To
			   )),
	send(F, layout).


:- pce_global(@graph_link, new(link(link, link, line(0,0,0,0,second)))).

display_arc(F, From:name, To:name) :->
	"Display arc From -> To"::
	get(F, node, From, NF),
	get(F, node, To, TF),
	send(NF, connect, TF, @graph_link).


node(F, Name:name, Node:graph_node) :<-
	"Get (create) node with specified name"::
	get(F, member, picture, Picture),
	(   get(Picture, member, Name, Node)
	->  true
	;   get(Picture, visible, area(X, Y, W, H)),
		MX is X + W,
		MY is Y + H,
		random(X, MX, NX),
		random(Y, MY, NY),
		send(Picture, display, new(Node, graph_node(Name)), point(NX, NY))
	).


:- pce_end_class.


:- pce_begin_class(graph_node(name), device).

handle(w/2, 0, link, link).

initialise(Node, Name:name) :->
	"Create from name"::
	send(Node, send_super, initialise),
	send(Node, display, circle(60), point(-30, -30)),
	send(Node, display, new(T, text(Name, center))),
	send(T, center, point(0, 0)),
	send(Node, send_super, name, Name).

name(Node, Name:name) :->
	"Change name of a node"::
	get(Node, member, text, Text),
	send(Text, string, Name),
	send(Node, send_super, name, Name).

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).

make_graph_node_recogniser(R) :-
	new(R, move_gesture(left)),
	send(R, condition,
	     ?(@event?position, distance, point(0,0)) < 30).


event(Node, Ev:event) :->
	"Make it movable"::
	(   send(@graph_node_recogniser, event, Ev)
	->  true
	;   send(Node, send_super, event, Ev)
	).

:- pce_end_class.

