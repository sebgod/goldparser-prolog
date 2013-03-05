:- module(symbol,
          [
           type/2,
           constant/2,
           by_type/4
        ]).

:- use_module(table, []).
:- use_module(item, []).

%%    type(+Number:int, +Name:atom) is det.
%%    type(?Number:int, ?Name:atom) is nondet.
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
%   This symbol is used to represent the end of the file or the end of the source input.
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

type(0, nonterminal).
type(1, terminal).
type(2, noise).
type(3, eof).
type(4, group_start).
type(5, ground_end).
type(6, decremented).
type(7, error).

constant(Kind, Constant) :- type(Kind, _), Constant = Kind.
constant(Kind, Constant) :- type(Constant, Kind).

by_type(Tables, Kind, SymbolIndex, Symbol) :-
    constant(Kind, Constant),
    table:items(symbol_table, Tables, SymbolIndex, Symbol),
    item:get(kind, Symbol, Constant).
