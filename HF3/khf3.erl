-module(khf3).
-author('czipobence@gmail.com').
-vsn('2016-10-28').
-export([megoldase/2]).
%-compile(export_all).

%% @spec khf3:megoldase(SSpec::sspec(), SSol::ssol()) -> B::bool().
%% B igaz, ha SSol megoldasa az SSpec feladvanynak.
megoldase({K,Mx},Sol) -> is_valid(K,Sol) and noConflict(K,Mx,Sol).

%% @spec khf3:boConflict(K::integer(), Mx::board(), Sol::ssol()) -> B::bool().
%% B igaz, ha Sol semmelyik mezoje nem mond ellent a megfelelo mezoinfonak Mx-ben.
noConflict(K,Mx,Sol) ->
	alltrue ( [ infosHold(nth_matr(Mx,Row,Col),Sol,Row,Col) || 
						Row <- lists:seq(1,K*K),
						Col <- lists:seq(1,K*K) ] ).


%% @spec khf3:infosHold(L::[info()], Sol::ssol(), Row::integer(), Col::integer()) -> B::bool().
%% B igaz, ha Sol Row soranak Col oszlopaban talalhato eleme nem mond ellent L infoinak
infosHold(L,Sol,Row,Col) ->
	alltrue ( [ infoHolds(I,Sol,Row,Col) || I <- L ] ).


%% @spec khf3:infoHolds(I::info(), Sol::ssol(), Row::integer(), Col::integer()) -> B::bool().
%% B igaz, ha Sol Row soranak Col oszlopaban talalhato eleme nem mond ellent I infonak
infoHolds(e,Sol,Row,Col) -> (nth_matr(Sol,Row,Col) rem 2 == 0);
infoHolds(o,Sol,Row,Col) -> (nth_matr(Sol,Row,Col) rem 2 == 1);
infoHolds(s,Sol,Row,Col) -> ((nth_matr(Sol,Row,Col) + nth_matr(Sol,Row+1,Col)) rem 2 == 1);
infoHolds(w,Sol,Row,Col) -> ((nth_matr(Sol,Row,Col) + nth_matr(Sol,Row,Col-1)) rem 2 == 1);
infoHolds(V,Sol,Row,Col) when is_number(V) -> (nth_matr(Sol,Row,Col) == V).

%% @spec khf3:is_valid(K::integer(),Sol::ssol()) -> B::bool().
%% B igaz, Sol onmagaban egy valid megoldasa egy sudoku feladvanynak.
is_valid(K,Sol) -> 
	alltrue ( [proplist(K*K,Row) || Row <- Sol]  ) and
	alltrue ( [proplist(K*K,getCol(Sol,C)) || C <- lists:seq(1,K*K)]) and
	alltrue ( [proplist(K*K,Sq)  || Sq <- feldarabolasa(Sol,{K,K})] ).

%% @spec khf3:proplist(N::integer(),L::[integer()]) -> B::bool().
%% B igaz, ha L lista 1-tol N-ig tartalmazza a szamokat, midnet pontosan egyszer.
proplist(N,Row) -> 
	(length(Row) == N) and
	alltrue([ contains(Row,X) || X <- lists:seq(1,N)]).

%% @spec khf3:alltrue(L::[any()]) -> B::bool().
%% B igaz, ha L lista minden eleme true.
alltrue([]) -> true;
alltrue([true|T]) -> alltrue(T);
alltrue([_|_]) -> false.

%% @spec khf3:contains(L::[any()], N::any()) -> B::bool().
%% B igaz, ha L lista tartalmazza N-t.
contains([],_) -> false;
contains([H|_],H) -> true;
contains([_|T],N) -> contains(T,N).

%% @spec khf3:contains(Mx::[[any()]], C::integer()) -> L::[any()].
%% L lista Mx matrix C. oszlopanak elemei
getCol(Mx,C) -> [ lists:nth(C,Row) || Row <- Mx ].

%------------------------------  KHF2 ------------------------------

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @spec khf3:nth_matr(M::matrix(), Row::integer(),Col::integer()) -> T::any().
%% T M matrix Row. soraban es Col. oszlopaban talalhato eleme
nth_matr(M, Row, Col) -> lists:nth(Col, lists:nth(Row, M)).

%------------------------------ /KHF2 ------------------------------

%------------------------------  KHF1 ------------------------------

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf3:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterû feldarabolása.
feldarabolasa(Mx,{W,H}) -> [flatten(L) || L <- split(Mx, W, H)].

%% @spec khf3:split(Mx::matrix(), W::subRows(), H::subCols()) -> LM::[matrix()]
%% Az LM lista az Mx mátrix {W,H} paraméterû darabolásakor keletkezõ
%% mátrixok listája
split(Mx, W, H) -> split(Mx, W,H, countW(Mx,W), countH(Mx,H)).

%% @spec khf3:split(Mx::matrix(), W::subRows(), H::subCols(), CW::integer(), CH::integer()) -> LM::[matrix()]
%% Az LM lista az Mx mátrix {W,H} paraméterû darabolásakor keletkezõ
%% mátrixok listája. A mátrixot oszlopai mentén CW, sorok mentén CH
%% darabra vágja, így a lista elemszáma CW * CH lesz.
split(Mx,W, H,CW,CH) -> 
	[submatrix(Mx,X*W+1,Y*H+1,W,H) ||
											X <- lists:seq(0, CW-1),
											Y <- lists:seq(0, CH-1)]. 

%% @spec khf3:submatrix(Mx::matrix(), StartX::integer(), StartY::integer(),Width::subRows(), Height::subCols()) -> M::matrix()
%% M mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartX oszloptól Width oszlop szélesen,
%% és a StartY sortól Height sor hosszan tart 									
submatrix(Mx, StartX, StartY, Width, Height) ->
	[lists:sublist(L, StartY, Height) || L <- lists:sublist(Mx, StartX, Width)].  

%% @spec khf3:flatten(Mx::matrix()) -> LL::[any()].
%% Az LL lista az Mx mátrix kilapítva
flatten(Mx) -> flatten(Mx, []).


%% @spec khf3:flatten(Mx::matrix(), L::[term()]) -> L1::[term()].
%% Az L1 lista az Mx mátrix kilapítva és L lista mögéfûzve
flatten([],L) -> L;
flatten([H|T], L) when is_list(H) -> flatten(H, flatten(T,L));
flatten([H|T], L) -> [H | flatten(T,L)].

%% @spec khf3:countW(L::[term()], W::integer()) -> CW::integer()
%% L listát W hosszú darabokra vágva CW darab lista keletkezik
countW(L, W) -> countW(L, W, 0, 0).


%% @spec khf3:countW(Mx::matrix(), H::integer()) -> CH::integer()
%% Mx mátrix oszlopait H hosszú darabra vágva CH darab oszlop keletkezik
countH([Hd|_], H) when is_list(Hd) -> countW(Hd, H).

%% @spec khf3:countW(L::[term()], W::integer(), Rem::integer(), Acc::integer()) -> CW::integer()
%% CW Acc hozzáadva ahhoz, hogy L lista Rem elemét elhagyva hány V
% hosszú darabra vághtató
countW([], _, _, Acc) -> Acc;
countW(L, W, 0, Acc) -> countW(L, W, W, Acc+1); 
countW([_|T], W, Rem, Acc) -> countW(T, W, Rem-1, Acc).

%------------------------------ /KHF1 ------------------------------
