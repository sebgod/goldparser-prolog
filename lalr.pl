:- module(lalr, [index/2, peek/3]).

:- use_module(stack, []).
:- use_module(table, []).

index(AST, LalrIndex) :-
    stack:peek(AST, s(LalrIndex, _)).

peek(Tables, AST, Lalr) :-
    index(AST, Index),
    table:item(lalr_table, Tables, Index, Lalr).

