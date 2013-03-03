:- module(read_primitive, [
               read_boolean/2,
               read_ascii_char/2,
               read_utf16le_z/2,
               read_ushort/2,
               read_byte/2
              ]).

read_boolean(Stream, Bool) :-
    read_byte(Stream, Byte),
    read_boolean_table(Byte, Bool).
read_boolean_table(0, false).
read_boolean_table(1, true).

read_ascii_char(Stream, Ascii) :-
    read_byte(Stream, Byte),
    char_code(Ascii, Byte).

read_utf16le_z(Stream, String) :-
    read_utf16le_z_acc(Stream, Codes),
    atom_codes(String, Codes).

read_utf16le_z_acc(Stream, [Code | Codes]) :-
    read_ushort(Stream, Code),
    Code > 0,
    !,
    read_utf16le_z_acc(Stream, Codes).
read_utf16le_z_acc(_, []).

read_ushort(Stream, UShort) :-
    read_byte(Stream, Low),
    read_byte(Stream, High),
    UShort is (High << 8) \/ Low.

read_byte(Stream, Byte) :-
    get_byte(Stream, Byte),
    Byte >= 0.
