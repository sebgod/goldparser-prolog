:- module(egt, [
                read_file/2,
                read_stream/2
               ]).

:- use_module(egt_primitive, []).
:- use_module(egt_record, []).
:- use_module(library(assoc)).
:- use_module(support).

read_file(Grammar, File) :-
    safe_open_file(File, read_stream(Grammar)).

read_stream(grammar(Header, Assoc), Stream) :-
    read_header(Stream, Header),
    empty_assoc(Empty),
    read_structures(Stream, Empty, Assoc).

%% read_structures(+Stream:stream)// is det.
% This is not a dcg but rather a convenient way of passing the
% changed AVL tress (library(assoc)).
read_structures(Stream) -->
    read_structure(Stream),
    !,
    read_structures(Stream).
read_structures(_, S, S).

read_header(Stream, header(Header)) :-
    egt_primitive:read_utf16le_z(Stream, Header).

read_structure(Stream) -->
    { egt_primitive:read_ascii_char(Stream, Type),
      egt_record:read_record(Stream, Type, Record),
      egt_record:sort_record(Record, Key, Value)
    },
    update(Key, Value).

update(Key, Value, PrevAssoc, NewAssoc) :-
    (   get_assoc(Key, PrevAssoc, OldValue)
    ->  NewValue = [Value | OldValue]
    ;   NewValue = [Value]
    ),
    put_assoc(Key, PrevAssoc, NewValue, NewAssoc).








