:- module(lalr, [index/2, get/3]).

:- use_module(stack, []).
:- use_module(table, []).

index(AST, LalrIndex) :-
    stack:peek(AST, s(LalrIndex, _)).

get(Tables, AST, Lalr) :-
    index(AST, Index),
    table:item(lalr_table, Tables, Index, Lalr).

