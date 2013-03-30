:- module(symbol,
          [
           type/3,
           by_type_name/4,
           token/4,
           characters/2,
           append/3
        ]).

:- use_module(table, []).
:- use_module(item, []).

%%  type(+Number:int, +Name:atom) is semidet.
%%  type(?Number:int, ?Name:atom) is nondet.
%
% Represents the symbol types used in the gold parser version 5.0.
%
% * nonterminal
%   Normal Nonterminal
%
% * terminal
%   Normal Terminal
%
% * noise
%   Noise terminal. These are ignored by the parser.
%   Comments and whitespace are considered 'noise'.
%
% * eof
%   End Character - End of File.
%   This symbol is used to represent the end of the file
%   or the end of the source input.
%
% * group_start
%   Lexical group start.
%
% * group_end
%   Lexical group end. Groups can end with normal terminals as well.
%
% * decremented
%   This was used in the CGT file format. It is not used in EGT.
%
% * error
%   Error Terminal. If the parser encounters an error reading a
%   token, this kind of symbol can used to differentiate it from other
%   terminal types.

type(0, nonterminal, 1).
type(1, terminal, 1).
type(2, noise, 1).
type(3, eof, 0).
type(4, group_start, 1).
type(5, group_end, 1).
type(6, decremented, 0).
type(7, error, 1).

%%	by_type_name(+Tables, ?Name, +SymbolIndex, ?Symbol) is semidet.
%%  by_type_name(+Tables, ?Name, ?SymbolIndex, ?Symbol) is nondet.
by_type_name(Tables, Name, SymbolIndex, Symbol) :-
    table:items(symbol_table, Tables, SymbolIndex, Symbol),
    (   item:value(kind, Symbol, KindId)
    ->  !
    ;   true
    ),
    type(KindId, Name, _).

by_index(Tables, SymbolIndex, Name, Args) :-
    must_be(integer, SymbolIndex),
    once(by_type_name(Tables, Name, SymbolIndex, _)),
    type(_, Name, Args).

%%	token(+Tables, +SymbolIndex, +Chars, -Token) is semidet.
token(Tables, SymbolIndex, Chars, SymbolIndex-Token) :-
    by_index(Tables, SymbolIndex, Name, Args),
    functor(Token, Name, Args),
    (   Args > 0
    ->  arg(1, Token, Chars)
    ;   true
    ).

%%	characters(+Symbol, -Characters) is det.
characters(Characters, Characters) :-
    atom(Characters), !.
characters(_-Data, Characters) :-
    functor(Data, _, Args),
    Args > 0,
    arg(1, Data, Characters).

append(SymbolIndex-Data0, Chars, SymbolIndex-DataN) :-
    functor(Data0, Name, Args),
    functor(DataN, Name, Args),
    (   Args > 0
    ->  arg(1, Data0, Chars0),
        atom_concat(Chars0, Chars, CharsN),
        arg(1, DataN, CharsN)
    ;   true
    ).











