
-module(khf1).
-author('czipobence@gmail.com').
-vsn('2016-09-22').
-export([feldarabolasa/2]).
%-compile(export_all).


%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterû feldarabolása.
feldarabolasa(Mx,{W,H}) -> [flatten(L) || L <- split(Mx, W, H)].

%% @spec khf1:split(Mx::matrix(), W::subRows(), H::subCols()) -> LM::[matrix()]
%% Az LM lista az Mx mátrix {W,H} paraméterû darabolásakor keletkezõ
%% mátrixok listája
split(Mx, W, H) -> split(Mx, W,H, countW(Mx,W), countH(Mx,H)).

%% @spec khf1:split(Mx::matrix(), W::subRows(), H::subCols(), CW::integer(), CH::integer()) -> LM::[matrix()]
%% Az LM lista az Mx mátrix {W,H} paraméterû darabolásakor keletkezõ
%% mátrixok listája. A mátrixot oszlopai mentén CW, sorok mentén CH
%% darabra vágja, így a lista elemszáma CW * CH lesz.
split(Mx,W, H,CW,CH) -> 
	[submatrix(Mx,X*W+1,W,Y*H+1,H) ||
											X <- lists:seq(0, CW-1),
											Y <- lists:seq(0, CH-1)]. 

%% @spec khf1:submatrix(Mx::matrix(), StartX::integer(), Width::subRows(), StartY::integer(), Height::subCols()) -> M::matrix()
%% M mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartX oszloptól Width oszlop szélesen,
%% és a StartY sortól Height sor hosszan tart 									
submatrix(Mx, StartX, Width, StartY, Height) ->
	[sublist(L, StartY, Height) || L <- sublist(Mx, StartX, Width)].

%% @spec khf1:sublist (L0::[term()], S::integer, N::integer) -> L::[term()]
%% Az L lista az L0 lista N hosszúságú részlistája az S. elemmel kezdve
sublist(_, 0, _) -> [];
sublist(_, _, 0) -> [];
sublist([], _, _) -> [];
sublist(L0, S, N) -> take( drop(L0, S-1),N). 

%% @spec khf1:take(L0::[term()], N::integer()) -> L::[term()]
%% Az L lista az L0 lista N hosszú prefixuma
take([], _) -> [];
take(_, 0) -> [];
take([H|T], N) -> [H | take(T, N-1)].

%% @spec khf1:drop(L0::[term()], N::integer()) -> L::[term()]
%% Az L0 lista olyan szuffixuma L, amely az L0 elsõ N elemét
%% nem tartalmazza
drop([],_) -> [];
drop(L0,0) -> L0;
drop([_|T], N) -> drop(T, N-1).  

%% @spec khf1:flatten(Mx::matrix()) -> LL::[any()].
%% Az LL lista az Mx mátrix kilapítva
flatten(Mx) -> flatten(Mx, []).


%% @spec khf1:flatten(Mx::matrix(), L::[term()]) -> L1::[term()].
%% Az L1 lista az Mx mátrix kilapítva és L lista mögéfûzve
flatten([],L) -> L;
flatten([H|T], L) when is_list(H) -> flatten(H, flatten(T,L));
flatten([H|T], L) -> [H | flatten(T,L)].

%% @spec khf1:countW(L::[term()], W::integer()) -> CW::integer()
%% L listát W hosszú darabokra vágva CW darab lista keletkezik
countW(L, W) -> countW(L, W, 0, 0).


%% @spec khf1:countW(Mx::matrix(), H::integer()) -> CH::integer()
%% Mx mátrix oszlopait H hosszú darabra vágva CH darab oszlop keletkezik
countH([Hd|_], H) when is_list(Hd) -> countW(Hd, H).

%% @spec khf1:countW(L::[term()], W::integer(), Rem::integer(), Acc::integer()) -> CW::integer()
%% CW Acc hozzáadva ahhoz, hogy L lista Rem elemét elhagyva hány V
% hosszú darabra vághtató
countW([], _, _, Acc) -> Acc;
countW(L, W, 0, Acc) -> countW(L, W, W, Acc+1); 
countW([_|T], W, Rem, Acc) -> countW(T, W, Rem-1, Acc).
