:- module(symbol, [
         type/2,
		 by_type/4
	]).

:- use_module(table, []).

%%  Normal Nonterminal
type(0, nonterminal).
%%	Normal Terminal
type(1, terminal).
%%	Noise terminal. These are ignored by the parser.
% Comments and whitespace are considered 'noise'.
type(2, noise).
%%	End Character - End of File.
% This symbol is used to represent the end of the file
% or the end of the source input.
type(3, eof).
%%	 Lexical group start.
type(4, group_start).
%%	Lexical group end. Groups can end with normal terminals as well.
type(5, ground_end).
%%	This was used in the CGT file format. It is not used in EGT.
type(6, decremented).
%%	Error Terminal. If the parser encounters an error reading a token,
% this kind of symbol can used to differentiate it from other terminal types
type(7, error).

by_type(Tables, Kind, SymbolIndex, Symbol) :-
	table:items(symbol_table, Tables, SymbolIndex, Symbol),
	get_assoc(kind, Symbol, Kind).
