:- module(graph_node, []).

:- use_module(library(pce)).

:- pce_begin_class(graph_node(name), device).

handle(w/2, 0, link, link).

initialise(Node, Name:name) :->
    "Create from name"::
    send(Node, send_super, initialise),
    Diameter = 60,
    Radius = Diameter / 2,
    send(Node, display, circle(Diameter), point(-Radius, -Radius)),
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

