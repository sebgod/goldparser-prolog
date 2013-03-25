:- module(support, [
                    list_trim/4,
                    list_skip/3,
                    char_and_code/3,
                    assoc_append_list/4,
                    safe_open_file/2,
                    safe_open_file/3,
                    must_be_assoc/1
                   ]).

:- meta_predicate
    safe_open_file(+, 1),
    safe_open_file(+, 1, +).

:- predicate_options(safe_open_file/3, 3,
                     [ encoding(encoding), type(oneof([text, binary])), pass_to(open/4, 4) ]).

must_be_assoc(Term) :-
    (    var(Term) ; \+ is_assoc(Term)),
    type_error(assoc, Term).
must_be_assoc(_).

list_trim(Source, Target, Trimmed, Rest) :-
    length(Source, Length),
    length(Trimmed, Length),
    append(Trimmed, Rest, Target).


list_skip(2, [_, _ | List], List) :- !.
list_skip(1, [_ | List], List) :- !.
list_skip(0, List, List) :- !.

list_skip(Skip, [_ | Rest0], RestN) :-
    Skip > 2,
    !,
    SkipN is Skip - 1,
    list_skip(SkipN, Rest0, RestN).

char_and_code(Input, Char, Code) :-
    (   integer(Input)
    ->  Code = Input
    ;   Char = Input
    ),
    char_code(Char, Code).

assoc_append_list(PrevAssoc, Key, Value, NewAssoc) :-
    (   get_assoc(Key, PrevAssoc, OldValue)
    ->  append(OldValue, [Value], NewValue)
    ;   NewValue = [Value]
    ),
    put_assoc(Key, PrevAssoc, NewValue, NewAssoc).

safe_open_file(File, StreamParser) :-
    safe_open_file(File, StreamParser, [type(binary)]).

safe_open_file(File, StreamParser, Options) :-
    setup_call_cleanup(
        open(File, read, Stream, Options),
        call(StreamParser, Stream),
        close(Stream)
                      ).
