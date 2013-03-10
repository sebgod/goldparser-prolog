:- module(action, [
                   type/2,
                   find/3,
                   list/2,
                   init_gotos/2,
                   update_gotos/3
                  ]).

:- use_module(item, []).

%%    type(+Number:int, +Name:atom) is det.
%%    type(?Number:int, ?Name:atom) is nondet.
%
% * shift
%   This action indicates the symbol is to be shifted.
%   The Target field will contain the index of the state
%   in the LALR State table that the parsing engine will advance to.
%   The Target field will contain the index
%   of the rule in the Rule Table.
%
% * reduce
%   This action denotes that the parser can reduce a rule.
%
% * goto
%   This action is used when a rule is reduced
%   and the parser jumps to the state that represents
%   the shifted nonterminal.
%   The Target field will contain the
%   index of the state in the LALR State table that the parsing engine
%   will jump to after a reduction if completed.
%
% * accept
%   When the parser encounters the Accept action for a given
%   symbol, the source text is accepted as correct and complete. In this
%   case, the Target field is not needed and should be ignored.

type(1, shift).
type(2, reduce).
type(3, goto).
type(4, accept).


find(Actions, SymbolIndex, Action) :-
    once((item:entry_member(Actions, Action, _ActionIndex),
          item:get(symbol_index, Action, SymbolIndex)
        )).

list(Actions, List) :-
    item:entries_to_list(Actions, List).

init_gotos(Actions, GotosN) :-
    item:empty(Gotos0),
    update_gotos(Actions, Gotos0, GotosN).

update_gotos(Actions, Gotos0, GotosN) :-
    type(GotoType, goto),
    findall(SymbolIndex-Target,
            (item:entry_member(Actions, Action, _),
             item:get(action, Action, GotoType),
             item:get(symbol_index, Action, SymbolIndex),
             item:get(target, Action, Target)
            ), GotoList),
    (   GotoList = []
    ->  GotosN = Gotos0
    ;   foldl(item:merge, GotoList, Gotos0, GotosN),
        debug(parser, '~p', gotosN(GotosN))
    ).






