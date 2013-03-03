:- module(ast, [empty/1 ,push/3, peek/2, pop/3]).

empty(ast([], [])).

push(ast(Stack, Tree), Terminal, ast([Terminal | Stack], Tree)).

peek(ast([Top | _], _Tree), Top).

pop(ast([Top | StackR], Tree), Top, ast(StackR, Tree)).
