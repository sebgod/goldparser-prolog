:- module(egt_record, [
                       sort_record/3,
                       read_record/3
                      ]).

:- use_module(egt_primitive, []).
:- use_module(support).

descriptor('c', character_set_table).
descriptor('D', dfa_table).
descriptor('g', group_table).
descriptor('I', initial_states).
descriptor('L', lalr_table).
descriptor('p', property).
descriptor('R', rule_table).
descriptor('S', symbol_table).
descriptor('t', table_counts).


structure_type(Letter, TableName, Props) :-
    descriptor(Letter, TableName),
    findall(Type-ItemName,
            item:property(TableName, ItemName, _, Type, _, _),
            Props),
    !.

structure_type(Letter, _, _) :-
    domain_error(descriptor/2, Letter).

sort_record(Record, Key, Structure) :-
    Record = record(multitype, _, Entries),
    [byte(Byte) | ValueEntries] = Entries,
    char_code(Char, Byte),
    fill_structure(Char, ValueEntries, Key, Structure).

fill_structure(Char, ValueEntries, Key, Structure) :-
    structure_type(Char, Key, Typings),
    list_trim(Typings, ValueEntries, Trimmed, Rest),
    maplist(fill_structure_acc, Typings, Trimmed, WithoutRest),
    fill_structure_rest(Rest, Key, [], RestStructure),
    (   RestStructure = []
    ->  UnsortedStructure = WithoutRest
    ;   UnsortedStructure = ['_entries'-RestStructure | WithoutRest]
    ),
    list_to_assoc(UnsortedStructure, Structure).

fill_structure_rest([], _, Structure, Structure).
fill_structure_rest([empty | Rest], Key) -->
    fill_structure_rest(Rest, Key).
fill_structure_rest(Rest, TableName, Structure0, Structure) :-
    findall(Type-ItemName,
            item:entry(TableName, ItemName, _, Type, _, _),
            Typings),
    list_trim(Typings, Rest, Trimmed, Next),
    maplist(fill_structure_acc, Typings, Trimmed, UnsortedRecord),
    list_to_assoc(UnsortedRecord, Record),
    append(Structure0, [Record], Structure1),
    fill_structure_rest(Next, TableName, Structure1, Structure).

fill_structure_acc(Type-Key, ValueEntry, Key-Value) :-
    ValueEntry =.. [Type, Value], !.

fill_structure_acc(Typing, ValueEntry, _) :-
    domain_error(Typing, ValueEntry).

read_record(Stream, 'M', record(multitype, NumberOfEntries, Entries)) :-
    egt_primitive:read_ushort(Stream, NumberOfEntries),
    (   NumberOfEntries >= 0
    ->  read_multitype_entries(Stream, NumberOfEntries, Entries)
    ),
    !.

read_record(_Stream, Unknown, _Record) :-
    domain_error(Unknown, oneof(['M'])).

read_multitype_entries(_, 0, []) :- !.

read_multitype_entries(Stream, NumberOfEntries, [Entry | Entries]) :-
    egt_primitive:read_ascii_char(Stream, Type),
    !,
    read_multitype_entry(Stream, Type, Entry),
    Remain is NumberOfEntries - 1,
    read_multitype_entries(Stream, Remain, Entries).

read_multitype_entry(_Stream, 'E', empty) :- !.

read_multitype_entry(Stream, 'B', boolean(Bool)) :- !,
    egt_primitive:read_boolean(Stream, Bool).

read_multitype_entry(Stream, 'b', byte(Byte)) :- !,
    egt_primitive:read_byte(Stream, Byte).

read_multitype_entry(Stream, 'I', short(Short)) :- !,
    egt_primitive:read_ushort(Stream, Short).

read_multitype_entry(Stream, 'S', string(String)) :- !,
    egt_primitive:read_utf16le_z(Stream, String).

read_multitype_entry(_Stream, Unknown, _Parsed) :- !,
    domain_error(Unknown, oneof(['E', 'B', b, 'I', 'S'])).

