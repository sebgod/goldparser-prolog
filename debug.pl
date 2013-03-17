:- module(debug, []).

:- debug.
:- doc_server(4000).    % Start PlDoc at port 4000
:- portray_text(true).

:- use_module(test_parser, []).

