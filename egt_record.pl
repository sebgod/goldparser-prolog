:- module(egt_record, [
               sort_record/3,
               read_record/3
              ]).

:- use_module(egt_primitive, []).
:- use_module(support).

descriptor('p', property,
           [
            short(index),
            string(name),
            string(value)
           ]).
descriptor('t', table_counts,
           [
            short(symbol_table),
            short(character_set_table),
            short(rule_table),
            short(dfa_table),
            short(lalr_table),
            short(group_table)
           ]).
descriptor('I', initial_states,
           [short(dfa), short(lalr)]
          ).
descriptor('c', character_set_table,
           [
            short(index),
            short(unicode_plane),
            short(range_count)
           ]).
descriptor('S', symbol_table,
           [
            short(index),
            string(name),
            short(kind)
           ]).
descriptor('g', group_table,
           [
            short(index),
            string(name),
            short(container_index),
            short(start_index),
            short(end_index),
            short(advance_mode),
            short(ending_mode)
           ]).
descriptor('R', rule_table,
           [
            short(index),
            short(head_index)
           ]).
descriptor('D', dfa_table,
           [
            short(index),
            boolean(accept_state),
            short(accept_index)
           ]).
descriptor('L', lalr_table, [short(index)]).

variable_part(rule_table, [short(symbol)]).
variable_part(lalr_table,
              [
               short(symbol_index),
               short(action),
               short(target)
              ]).
variable_part(dfa_table,
              [
               short(character_set_index),
               short(target_index)
              ]).
variable_part(character_set_table,
              [
               short(start_character),
               short(end_character)
              ]).
variable_part(group_table, [short(group_index)]).

structure_type(Letter, Name, Props) :-
    descriptor(Letter, Name, Props),
    !.

structure_type(Letter, _, _) :-
    throw(error('Unknown structure type',
                context(structure_type/3, Letter)
               )
         ).

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
fill_structure_rest(Rest, Key, Structure0, Structure) :-
    variable_part(Key, Typings),
    list_trim(Typings, Rest, Trimmed, Next),
    maplist(fill_structure_acc, Typings, Trimmed, UnsortedRecord),
    list_to_assoc(UnsortedRecord, Record),
    append(Structure0, [Record], Structure1),
    fill_structure_rest(Next, Key, Structure1, Structure).

fill_structure_acc(Typing, ValueEntry, Key-Value) :-
    Typing =.. [Type, Key],
    ValueEntry =.. [Type, Value], !.

fill_structure_acc(Typing, ValueEntry, _) :-
    throw(error('Wrong typing',
                context(structure_type/3, [Typing, ValueEntry])
               )
         ).

read_record(Stream, 'M', record(multitype, NumberOfEntries, Entries)) :-
    !,
    egt_primitive:read_ushort(Stream, NumberOfEntries),
    read_multitype_entries(Stream, NumberOfEntries, Entries).

read_record(_Stream, Unknown, _Record) :-
    throw(error('Unknown record', context(read_record/3, Unknown))).

read_multitype_entries(Stream, NumberOfEntries, [Entry | Entries]) :-
    NumberOfEntries > 0,
    egt_primitive:read_ascii_char(Stream, Type),
    !,
    read_multitype_entry(Stream, Type, Entry),
    Remain is NumberOfEntries - 1,
    read_multitype_entries(Stream, Remain, Entries).
read_multitype_entries(_, 0, []).

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
    throw(error('Unknown multitype',
                context(read_multitype_entry/3, Unknown))).


