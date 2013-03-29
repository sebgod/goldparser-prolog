:- module(charset, [member/2]).

:- use_module(item, []).

member(Charset, Code) :-
    item:entries(Charset, Ranges),
    item:entries_size(Ranges, Size),
    member(Ranges, Code, 1, Size).

member(Ranges, Code, Left, Right) :-
    Right >= Left,
    Middle is (Right + Left) // 2,
    arg(Middle, Ranges, Range),
    item:entry_value(start_character, Range, Start),
    item:entry_value(end_character, Range, End),
    (   between(Start, End, Code)
    ->  !
    ;   Left < Right,
        (   Code < Start
        ->  Left1 is Left, Right1 is Middle
        ;   Left1 is Middle + 1, Right1 is Right
        ),
        member(Ranges, Code, Left1, Right1)
    ).
