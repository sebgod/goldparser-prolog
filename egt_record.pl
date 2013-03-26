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

item(character_set_table, index,          0, short, item(V, _, _), V).
item(character_set_table, unicode_plane,  1, short, item(_, V, _), V).
item(character_set_table, range_count,    2, short, item(_, _, V), V).

item(dfa_table, index,        0, short,   item(V, _, _), V).
item(dfa_table, accept_state, 1, boolean, item(_, V, _), V).
item(dfa_table, accept_index, 2, short,   item(_, _, V), V).

item(group_table, index,          0, short,  item(V, _, _, _, _, _, _), V).
item(group_table, name,           1, string, item(_, V, _, _, _, _, _), V).
item(group_table, container_index, 2, short, item(_, _, V, _, _, _, _), V).
item(group_table, start_index,    3, short,  item(_, _, _, V, _, _, _), V).
item(group_table, end_index,      4, short,  item(_, _, _, _, V, _, _), V).
item(group_table, advance_mode,   5, short,  item(_, _, _, _, _, V, _), V).
item(group_table, ending_mode,    6, short,  item(_, _, _, _, _, _, V), V).

item(initial_states, dfa,  0, short, item(V, _), V).
item(initial_states, lalr, 1, short, item(_, V), V).

item(lalr_table, index, 0, short, item(V, _), V).

item(property, index, 0, short,  item(V, _, _), V).
item(property, name,  1, string, item(_, V, _), V).
item(property, value, 2, string, item(_, _, V), V).

item(rule_table, index,      0, short, item(V, _), V).
item(rule_table, head_index, 1, short, item(_, V), V).

item(symbol_table, index, 0, short,  item(V, _, _), V).
item(symbol_table, name,  1, string, item(_, V, _), V).
item(symbol_table, kind,  2, short,  item(_, _, V), V).

item(table_counts, symbol_table, 0, short, item(V, _, _, _, _, _), V).
item(table_counts, character_set_table,
     1, short, item(_, V, _, _, _, _), V).
item(table_counts, rule_table,   2, short, item(_, _, V, _, _, _), V).
item(table_counts, dfa_table,    3, short, item(_, _, _, V, _, _), V).
item(table_counts, lalr_table,   4, short, item(_, _, _, _, V, _), V).
item(table_counts, group_table,  5, short, item(_, _, _, _, _, V), V).

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

structure_type(Letter, TableName, Props) :-
    descriptor(Letter, TableName),
    findall(Type-ItemName,
            item(TableName, ItemName, _, Type, _, _),
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
fill_structure_rest(Rest, Key, Structure0, Structure) :-
    variable_part(Key, Typings),
    list_trim(Typings, Rest, Trimmed, Next),
    maplist(fill_structure_acc, Typings, Trimmed, UnsortedRecord),
    list_to_assoc(UnsortedRecord, Record),
    append(Structure0, [Record], Structure1),
    fill_structure_rest(Next, Key, Structure1, Structure).

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
    throw(error('Invalid record', context(read_record/3, Unknown))).

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
    throw(error('Unknown multitype',
                context(read_multitype_entry/3, Unknown))).


