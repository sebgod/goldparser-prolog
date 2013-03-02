:- module(ast, [
				empty/1
			   ,push/3]).

empty(ast([], [])).

push(ast(Stack, Tree), Terminal, ast([Terminal | Stack], Tree)).
