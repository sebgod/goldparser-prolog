:- module(graph_viewer, []).

:- use_module(library(pce)).

:- require([ forall/2, random/3]).

:- pce_autoload(graph_node, graph_node).

:- pce_begin_class(graph_viewer, frame).

:- meta_predicate generate(+, +, 3).

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

generate(F, Graph:prolog) :->
    "Create graph using generator"::
    send(F, clear),
    (    call(Graph, From, To),
         send(F, display_arc, From, To),
         fail
    ;    true
    ),
    send(F, layout).


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
