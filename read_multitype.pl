:- module(read_multitype, [
               sort_record/3,
               read_record/3
              ]).

:- use_module(read_primitive).
:- use_module(support).

descriptor('p', property,
           [
            (index, short),
            (name, string),
            (value, string)
           ]).
descriptor('t', table_counts,
           [
            (symbol_table, short),
            (character_set_table, short),
            (rule_table, short),
            (dfa_table, short),
            (lalr_table, short),
            (group_table, short)
           ]).
descriptor('I', initial_states,
           [(dfa, short), (lalr, short)]
          ).
descriptor('c', character_set_table,
           [
            (index, short),
            (unicode_plane, short),
            (range_count, short)
           ]).
descriptor('S', symbol_table,
           [
            (index, short),
            (name, string),
            (kind, short)
           ]).
descriptor('R', rule_table,
           [
            (index, short),
            (head_index, short)
           ]).
descriptor('D', dfa_table,
           [
            (index, short),
            (accept_state, boolean),
            (accept_index, short)
           ]).
descriptor('L', lalr_table, [(index, short)]).

variable_part(rule_table, [(symbol, short)]).
variable_part(lalr_table,
              [
               (symbol_index, short),
               (action, short),
               (target, short)
              ]).
variable_part(dfa_table,
              [
               (character_set_index, short),
               (target_index, short)
              ]).
variable_part(character_set_table,
              [
               (start_character, short),
               (end_character, short)
              ]).

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

fill_structure_acc((Key, Type), ValueEntry, Key-Value) :-
    ValueEntry =.. [Type, Value].

read_record(Stream, 'M', record(multitype, NumberOfEntries, Entries)) :-
    !,
    read_ushort(Stream, NumberOfEntries),
    read_multitype_entries(Stream, NumberOfEntries, Entries).

read_record(_Stream, Unknown, _Record) :-
    throw(error('Unknown record', context(read_record/3, Unknown))).

read_multitype_entries(Stream, NumberOfEntries, [Entry | Entries]) :-
    NumberOfEntries > 0,
    read_ascii_char(Stream, Type),
    !,
    read_multitype_entry(Stream, Type, Entry),
    Remain is NumberOfEntries - 1,
    read_multitype_entries(Stream, Remain, Entries).
read_multitype_entries(_, 0, []).

read_multitype_entry(_Stream, 'E', empty) :- !.

read_multitype_entry(Stream, 'B', boolean(Bool)) :- !,
    read_boolean(Stream, Bool).

read_multitype_entry(Stream, 'b', byte(Byte)) :- !,
    read_byte(Stream, Byte).

read_multitype_entry(Stream, 'I', short(Short)) :- !,
    read_ushort(Stream, Short).

read_multitype_entry(Stream, 'S', string(String)) :- !,
    read_utf16le_z(Stream, String).

read_multitype_entry(_Stream, Unknown, _Parsed) :- !,
    throw(error('Unknown multitype',
                context(read_multitype_entry/3, Unknown))).


