:- module(symbol,
          [
           type/3,
           by_type_id/4,
           by_type_name/4,
           token/4
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

type(0, nonterminal, 1).
type(1, terminal, 1).
type(2, noise, 1).
type(3, eof, 0).
type(4, group_start, 1).
type(5, ground_end, 1).
type(6, decremented, 0).
type(7, error, 1).

by_type_name(Tables, KindName, SymbolIndex, Symbol) :-
    by_type_id(Tables, KindId, SymbolIndex, Symbol),
    type(KindId, KindName, _).

by_type_id(Tables, KindId, SymbolIndex, Symbol) :-
    table:items(symbol_table, Tables, SymbolIndex, Symbol),
    item:get(kind, Symbol, KindId),
    type(KindId, _, _).

token(Tables, SymbolIndex, Data, SymbolIndex-Token) :-
    by_type_name(Tables, KindName, SymbolIndex, _),
    type(_, KindName, Args),
    functor(Token, KindName, Args),
    (   Args > 0
    ->  arg(1, Token, Data)
    ;   true
    ).

