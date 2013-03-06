:- module(stack, [empty/1 ,push/3, peek/2, pop/3, rpop/4]).

empty([]).

push(Stack, Terminal, [Terminal | Stack]).

peek([Top | _], Top).

pop([Top | StackR], Top, StackR).

rpop(Stack, Size, Items, StackR) :-
    length(ReversedList, Size),
    append(ReversedList, StackR, Stack),
    reverse(ReversedList, Items).

