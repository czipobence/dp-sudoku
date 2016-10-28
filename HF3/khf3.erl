-module(khf3).
-author('czipobence@gmail.com').
-vsn('2016-10-28').
%-export([megoldase/2]).
-compile(export_all).

%% @spec khf3:megoldase(SSpec::sspec(), SSol::ssol()) -> B::bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.
megoldase({K,Mx},Sol) -> is_valid(K,Sol).

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

%% @spec khf3:forall(P::function(), L::[any()]) -> B::bool().
%% B igaz, ha L listanak minden elemere igaz P predikatum.
forall(P,L) -> lists:foldr(fun(X,Acc)-> X and Acc end,true,lists:map(P,L)). %TODO optimize

%% @spec khf3:exists(P::function(), L::[any()]) -> B::bool().
%% B igaz, ha L listanak letezik eleme, melyre igaz P predikatum.
exists(P,L) -> lists:foldr(fun(X,Acc)-> X or Acc end,false,lists:map(P,L)). %TODO optimize

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

%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @spec khf3:ertekek(SSpec::sspec(), R_C::coords()) -> Vals::[integer()]
%%   Egy érték pontosan akkor szerepel a Vals listában, ha teljesíti a
%%   fenti Prolog specifikációban felsorolt (a), (b) és (c) feltételeket, ahol
%%   Vals az SSpec specifikációval megadott Sudoku-feladvány R_C
%%   koordinátájú mezőjében megengedett értékek listája.
ertekek({K, M}, {R,C}) -> 
	Val = nth_matr(M,R,C),
	Mx = set_nth_matr(M,R,C,[]),
	AllNum = lists:seq(1,K*K),
	L = merge(flatten(get_row(Mx,R)), merge(flatten(get_col(Mx,C)), flatten(submatrix(Mx, top(R,K), top(C,K) ,K,K)))),
	Allowed = lists:foldr(fun(X,Acc)->intersection(X,Acc) end, AllNum, lists:map(fun(X) -> allowed_by(X,K) end,L)),
	Limited = lists:foldr(fun(X,Acc)->intersection(X,Acc) end, AllNum, lists:map(fun(X) -> limited_to(X,K) end,Val)),
	intersection(Allowed,Limited).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @spec khf3:nth_matr(M::matrix(), Row::integer(),Col::integer()) -> T::any().
%% T M matrix Row. soraban es Col. oszlopaban talalhato eleme
nth_matr(M, Row, Col) -> lists:nth(Col, lists:nth(Row, M)).

%% @spec khf3:set_nth_matr(M::matrix(), Row::integer(),Col::integer(),To::any()) -> Mx::matrix().
%% Mx olyan matrix, ami megegyezik M-el, kiveve Row soranak Col oszlopa, ahol az elem To
set_nth_matr(M, Row,Col,To) -> [
		[set_nth_proc_element(nth_matr(M,R,C), R, C, Row, Col, To) 
						|| C <- lists:seq(1,length(hd(M)))] 
				|| R <- lists:seq(1,length(M))].

%% @spec khf3:set_nth_proc_element(E::any(), R::integer(),C::integer(), Row::integer(), Col::integer(),To::any()) -> E1::matrix().
%% E1 az E, kiveve ha Row = R es Col = C, mert akkor To
set_nth_proc_element(_,R,C,R,C,To) -> To;
set_nth_proc_element(E,_,_,_,_,_) -> E.

%% @spec khf3:intersection(L1::[any()], L2::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete

intersection(L1,L2) -> lists:reverse(intersection(L1,L2,[])).
%% @spec khf3:intersection(L1::[any()], L2::[any()], Coll::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete osszefuzve Coll-al
intersection([],_,Coll) -> Coll;
intersection(_, [], Coll) -> Coll;
intersection([H|T1], [H|T2], Coll) -> intersection(T1,T2,[H|Coll]);
intersection([H1|T1], [H2|T2], Coll) when H1 < H2 -> 
		intersection(T1, [H2|T2], Coll);
intersection([H1|T1], [_|T2], Coll) -> 
		intersection([H1|T1], T2, Coll).



%% @type info()  = e | o | s | w | integer().
%% @spec khf3:limited_to(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru sudoku adott mezojeben lehetnek
limited_to(V,K) when is_number(V) -> 
	case V < K*K+1 of
	true -> [V];
	false -> []
	end;
limited_to(o,K) -> lists:reverse(secs_till(K*K,1,[]));
limited_to(e,K) -> lists:reverse(secs_till(K*K,2,[]));
limited_to(_,K) -> lists:seq(1,K*K).

%% @spec khf3:allowed_by(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru 
%% sudokuban az adott mezovel egy szegmensben (K*K negyzet, sor, oszlop)
%% engedelyezettek
allowed_by(V,K)when is_number(V) -> lists:reverse(miss(V,K*K));
allowed_by(_,K) -> lists:seq(1,K*K).

%% @spec khf3:secs_till(N::integer(), I::integer(), L::[integer()]) -> L1::[integer()].
%% L lista I tol kezdve N-ig minden masodik szam listaja, megforditva L ele fuzva
secs_till(N,I,L) ->
	case I < N+1 of
	true -> secs_till(N,I+2,[I|L]); 
	false -> L
	end.

%% @spec khf3:miss(V::integer(), N::integer()) -> L::[integer()].
%% L lista N tol 1-ig a szamok, V-t kihagyva
miss(V,N) -> miss(V,N,1,[]).

%% @spec khf3:miss(V::integer(), N::integer(), Curr:integer(), L:[integer()]) -> L1::[integer()].
%% L1 lista N-tol Curr-ig a szamok, V-t kihagyva, L ele fuzve
miss(N,N,N,L) -> L;
miss(_,N,N,L) -> [N|L];
miss(V,N,V,L) -> miss(V,N,V+1,L);
miss(V,N,I,L) -> miss(V,N,I+1, [I|L]).

%% @spec khf3:get_row(Mx::matrix(), Row::integer()) -> L::[any()].
%% Az L lista Mx matrix Row. oszlopa
get_row(Mx, Row) -> lists:nth(Row, Mx).

%% @spec khf3:get_col(Mx::matrix(), Col::integer()) -> L::[any()].
%% Az L lista Mx matrix Col. oszlopanak elemei (oszlop helyett listava lapitva)
get_col(Mx, Col) -> [lists:nth(Col,L) || L <- Mx].


%% @spec khf3:top(L0::[any()], L1::[any()]) -> L::[any()].
%% L lista L0 es L1 osszefuzve
merge([],L) -> L;
merge([H|T], L) -> merge(T, [H|L]).

%% @spec khf3:top(R::integer(), K::integer()) -> Top::integer().
%% Top a legnagyobb, R-nel kisebb K-val oszthato poz. eg. szam
top(R,K) -> top(R,K,1).
top(R,K,Acc) ->
	case Acc + K > R of
	true -> Acc;
	false -> top(R,K,Acc+K)
	end.


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

%% @spec khf3:sublist (L0::[term()], S::integer, N::integer) -> L::[term()]
%% Az L lista az L0 lista N hosszúságú részlistája az S. elemmel kezdve
sublist(_, 0, _) -> [];
sublist(_, _, 0) -> [];
sublist([], _, _) -> [];
sublist(L0, S, N) -> take( drop(L0, S-1),N). 

%% @spec khf3:take(L0::[term()], N::integer()) -> L::[term()]
%% Az L lista az L0 lista N hosszú prefixuma
take([], _) -> [];
take(_, 0) -> [];
take([H|T], N) -> [H | take(T, N-1)].

%% @spec khf3:drop(L0::[term()], N::integer()) -> L::[term()]
%% Az L0 lista olyan szuffixuma L, amely az L0 elsõ N elemét
%% nem tartalmazza
drop([],_) -> [];
drop(L0,0) -> L0;
drop([_|T], N) -> drop(T, N-1).  

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
