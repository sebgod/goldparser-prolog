:- module(symbol, [
         symbol_type/2,
		 symbols_of_kind/4
	]).

:- use_module(parser_members).

symbol_type(0, nonterminal). % Normal Nonterminal
symbol_type(1, terminal).    % Normal Terminal
symbol_type(2, noise).       % Noise terminal. These are ignored by the parser. Comments and whitespace are considered 'noise'.
symbol_type(3, eof).         % End Character - End of File. This symbol is used to represent the end of the file or the end of the source input.
symbol_type(4, group_start). % Lexical group start.
symbol_type(5, ground_end).  % Lexical group end. Groups can end with normal terminals as well.
symbol_type(6, decremented). % This was used in the CGT file format. It is not used in EGT.
symbol_type(7, error).		 % Error Terminal. If the parser encounters an error reading a token, this kind of symbol can used to differentiate it from other terminal types.

symbols_of_kind(Tables, Kind, SymbolIndex, Symbol) :-
	table_items(symbol_table, Tables, SymbolIndex, Symbol),
	get_assoc(kind, Symbol, Kind).
