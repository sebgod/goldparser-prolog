:- module(stack, [empty/1 ,push/3, peek/2, pop/3, rpop/4]).

empty([]).

push(Stack, Symbol, [Symbol | Stack]).

peek([Top | _], Top).

pop([Top | StackR], Top, StackR).

rpop(Stack, Size, Symbols, StackR) :-
    length(ReversedList, Size),
    append(ReversedList, StackR, Stack),
    reverse(ReversedList, Symbols).

