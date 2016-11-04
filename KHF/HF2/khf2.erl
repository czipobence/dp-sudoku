-module(khf2).
-author('czipobence@gmail.com').
-vsn('2016-10-13').
-export([ertekek/2]).
%-compile(export_all).

%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @spec khf2:ertekek(SSpec::sspec(), R_C::coords()) -> Vals::[integer()]
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
%% @spec khf2:nth_matr(M::matrix(), Row::integer(),Col::integer()) -> T::any().
%% T M matrix Row. soraban es Col. oszlopaban talalhato eleme
nth_matr(M, Row, Col) -> lists:nth(Col, lists:nth(Row, M)).

%% @spec khf2:set_nth_matr(M::matrix(), Row::integer(),Col::integer(),To::any()) -> Mx::matrix().
%% Mx olyan matrix, ami megegyezik M-el, kiveve Row soranak Col oszlopa, ahol az elem To
set_nth_matr(M, Row,Col,To) -> [
		[set_nth_proc_element(nth_matr(M,R,C), R, C, Row, Col, To) 
						|| C <- lists:seq(1,length(hd(M)))] 
				|| R <- lists:seq(1,length(M))].

%% @spec khf2:set_nth_proc_element(E::any(), R::integer(),C::integer(), Row::integer(), Col::integer(),To::any()) -> E1::matrix().
%% E1 az E, kiveve ha Row = R es Col = C, mert akkor To
set_nth_proc_element(_,R,C,R,C,To) -> To;
set_nth_proc_element(E,_,_,_,_,_) -> E.

%% @spec khf2:intersection(L1::[any()], L2::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete

intersection(L1,L2) -> lists:reverse(intersection(L1,L2,[])).
%% @spec khf2:intersection(L1::[any()], L2::[any()], Coll::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete osszefuzve Coll-al
intersection([],_,Coll) -> Coll;
intersection(_, [], Coll) -> Coll;
intersection([H|T1], [H|T2], Coll) -> intersection(T1,T2,[H|Coll]);
intersection([H1|T1], [H2|T2], Coll) when H1 < H2 -> 
		intersection(T1, [H2|T2], Coll);
intersection([H1|T1], [_|T2], Coll) -> 
		intersection([H1|T1], T2, Coll).



%% @type info()  = e | o | s | w | integer().
%% @spec khf2:limited_to(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru sudoku adott mezojeben lehetnek
limited_to(V,K) when is_number(V) -> 
	case V < K*K+1 of
	true -> [V];
	false -> []
	end;
limited_to(o,K) -> lists:reverse(secs_till(K*K,1,[]));
limited_to(e,K) -> lists:reverse(secs_till(K*K,2,[]));
limited_to(_,K) -> lists:seq(1,K*K).

%% @spec khf2:allowed_by(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru 
%% sudokuban az adott mezovel egy szegmensben (K*K negyzet, sor, oszlop)
%% engedelyezettek
allowed_by(V,K)when is_number(V) -> lists:reverse(miss(V,K*K));
allowed_by(_,K) -> lists:seq(1,K*K).

%% @spec khf2:secs_till(N::integer(), I::integer(), L::[integer()]) -> L1::[integer()].
%% L lista I tol kezdve N-ig minden masodik szam listaja, megforditva L ele fuzva
secs_till(N,I,L) ->
	case I < N+1 of
	true -> secs_till(N,I+2,[I|L]); 
	false -> L
	end.

%% @spec khf2:miss(V::integer(), N::integer()) -> L::[integer()].
%% L lista N tol 1-ig a szamok, V-t kihagyva
miss(V,N) -> miss(V,N,1,[]).

%% @spec khf2:miss(V::integer(), N::integer(), Curr:integer(), L:[integer()]) -> L1::[integer()].
%% L1 lista N-tol Curr-ig a szamok, V-t kihagyva, L ele fuzve
miss(N,N,N,L) -> L;
miss(_,N,N,L) -> [N|L];
miss(V,N,V,L) -> miss(V,N,V+1,L);
miss(V,N,I,L) -> miss(V,N,I+1, [I|L]).

%% @spec khf2:get_row(Mx::matrix(), Row::integer()) -> L::[any()].
%% Az L lista Mx matrix Row. oszlopa
get_row(Mx, Row) -> lists:nth(Row, Mx).

%% @spec khf2:get_col(Mx::matrix(), Col::integer()) -> L::[any()].
%% Az L lista Mx matrix Col. oszlopanak elemei (oszlop helyett listava lapitva)
get_col(Mx, Col) -> [lists:nth(Col,L) || L <- Mx].


%% @spec khf2:top(L0::[any()], L1::[any()]) -> L::[any()].
%% L lista L0 es L1 osszefuzve
merge([],L) -> L;
merge([H|T], L) -> merge(T, [H|L]).

%% @spec khf2:top(R::integer(), K::integer()) -> Top::integer().
%% Top a legnagyobb, R-nel kisebb K-val oszthato poz. eg. szam
top(R,K) -> top(R,K,1).
top(R,K,Acc) ->
	case Acc + K > R of
	true -> Acc;
	false -> top(R,K,Acc+K)
	end.

%% @spec khf2:submatrix(Mx::matrix(), StartX::integer(), StartY::integer(),Width::subRows(), Height::subCols()) -> M::matrix()
%% M mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartX oszloptól Width oszlop szélesen,
%% és a StartY sortól Height sor hosszan tart 									
submatrix(Mx, StartX, StartY, Width, Height) ->
	[lists:sublist(L, StartY, Height) || L <- lists:sublist(Mx, StartX, Width)].
	
%% @spec khf2:flatten(Mx::matrix()) -> LL::[any()].
%% Az LL lista az Mx mátrix kilapítva
flatten(Mx) -> flatten(Mx, []).


%% @spec khf2:flatten(Mx::matrix(), L::[term()]) -> L1::[term()].
%% Az L1 lista az Mx mátrix kilapítva és L lista mögéfûzve
flatten([],L) -> L;
flatten([H|T], L) when is_list(H) -> flatten(H, flatten(T,L));
flatten([H|T], L) -> [H | flatten(T,L)].

