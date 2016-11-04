% :- pred megoldase(sspec::in, ssol::in).
% megoldase(+SSpec,+SSol) : sikeres, ha az SSol érték-mátrix megoldása az SSpec Sudoku-feladványnak.
megoldase(s(K,Mx),Sol) :-
	valid(K,Sol),
	fitting(K,Mx,Sol).

% fitting(K,Mx,Sol) : sikeres ha Sol nem mond ellent a mezoinfoknak
fitting(K,Mx,Sol) :-
	N is K*K,
	myforall(
		(bet(1,N,Col),bet(1,N,Row)), 
		(mx_item(Mx,Row,Col,Infos),	infosHold(Infos,Sol,Row,Col))
	).

% infosHold(L,Sol,Row,Col) : sikres, ha Sol Row. soranak Col. eleme kielegiti L infolista minden elemet.
infosHold([H|T],Sol,Row,Col) :-
	infoHolds(H,Sol,Row,Col),
	infosHold(T,Sol,Row,Col).

infosHold([],_,_,_).

% infosHold(I,Sol,Row,Col) : sikres, ha Sol Row. soranak Col. eleme kielegiti I infot.
infoHolds(v(V),Sol,Row,Col) :-
	mx_item(Sol,Row,Col,V).
infoHolds(e,Sol,Row,Col) :-
	mx_item(Sol,Row,Col,V),
	0 is V mod 2.
infoHolds(o,Sol,Row,Col) :-
	mx_item(Sol,Row,Col,V),
	1 is V mod 2.
infoHolds(w,Sol,Row,Col) :-
	mx_item(Sol,Row,Col,V1),
	Col1 is Col - 1,
	mx_item(Sol,Row,Col1,V2),
	Sum is V1 + V2,
	1 is Sum mod 2.
infoHolds(s,Sol,Row,Col) :-
	mx_item(Sol,Row,Col,V1),
	Row1 is Row + 1,
	mx_item(Sol,Row1,Col,V2),
	Sum is V1 + V2,
	1 is Sum mod 2.

% valid(K,Sol) sikeres, ha Sol egy valid K parameteru sudoku kitoltes
valid(K,Sol) :- 
	valid_ll(K,Sol),
	N is K * K,
	feldarabolasa(Sol,N-1,ColL),
	valid_ll(K,ColL),
	feldarabolasa(Sol,K-K,SqL),
	valid_ll(K,SqL).

% valid_ll(K,LL) sikeres, ha LL minden eleme 1 tol K*K ig tartalmazza a szamok
% egy permutaciojat
valid_ll(K,[H|T]) :-
	valid_ll(K,T),
	N is K * K,	
	valid_list(N,H).
valid_ll(_,[]).
	

% valid_list(K,L) sikeres, ha L minden elemei a 1 tol K*K ig a szamok
% egy permutacioja
valid_list(N,L) :-
	length(L,N),
	myforall(bet(1,N,X), member(X,L)).

% myforall(A,B) sikeres, ha minden A ra B is igaz
myforall(A, B) :-
    \+ (call(A), \+ call(B)).	
	
%------------------------------   KHF2 ------------------------------

%mx_item(Mx,R,C,Val)
%Val Mx matrix R sorának C oszlopában talalhato eleme
mx_item(Mx,R,C,Val) :-
	list_nth(Mx,R,TMP),
	list_nth(TMP,C,Val).

%list_nth(L,N,Val)
%Val L lista N. eleme (1tol kezdve a szamozast)
list_nth([H|_],1,H).
list_nth([_|T],N,Val):-
	N > 1,
	N1 is N-1,
	list_nth(T,N1,Val).

%bet(From,To,N)
%N From es To kozotti szam
bet(From,To,N) :-
	From =< To, N = From.
bet(From,To,N) :-
	From < To,
	From1 is From + 1,
	bet(From1,To,N).

%------------------------------  /KHF2 ------------------------------
%------------------------------   KHF1 ------------------------------

% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).
% feldarabolasa(Mx, P, LL): Az LL lista az Mx mátrix P paraméterű feldarabolása.
feldarabolasa(Mx, R-C, LL) :- 
	feldarabolasa(Mx,R,C,LLt),
	flattenNextLvl(LLt,LL).


% feldarabolasa(Mx, R,C, LL): Az LL lista az Mx mátrix R-C paraméterű mátrixokra darabolása
feldarabolasa(Mx, R, C, LL):- 
	countR(Mx, R, Cr, 0),
	countC(Mx, C, Cc, 0),
	split(Mx, R, C, Cr, Cc, LL).


% split(Mx, R,C, Cr, Cc, LL): Az LL lista az Mx mátrix R-C paraméterű mátrixokra darabolása,
% sorok mentén legfeljebb Cr, oszolopok mentén legfeljebb Cc részre vágva
split([Hmx|Tmx], R,  C, Cr, Cc, LL) :-
	split([Hmx|Tmx], R, C, Cr, Cc, 0, 0, LL).

% split(Mx, R,C, Cr, Cc, Cntr, Cntc LL): Az LL lista az Mx mátrix R-C paraméterű mátrixokra darabolása,
% sorok mentén legfeljebb Cr, oszolopok mentén legfeljebb Cc részre vágva
% a sorok szerint Cntr, az oszlopok szerint Cntc-dik részmátrixtól kezdve
split(_, _, _, Cr, _, Cr, 0, []).
split([H|T],R,C,Cr,Cc,Cntr,Cc,L) :-
	Cntr1 is Cntr+1,
	split([H|T],R,C,Cr,Cc,Cntr1,0,L).
split([Hmx|Tmx], R,  C, Cr, Cc, Cntr, Cntc, [H1|T1]) :- 
	Cntr < Cr,
	Cntc < Cc,
	Cntc1 is Cntc + 1,
	StartC is Cntc * C + 1,
	StartR is Cntr * R + 1,
	submatrix([Hmx|Tmx], StartC, C, StartR, R, H1),
	split([Hmx|Tmx], R, C, Cr, Cc, Cntr, Cntc1, T1).

% submatrix(Mx, StartC, Width, StartR, Height, Smx):
%% Smx mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartC oszloptól Width oszlop szélesen,
%% és a StartR sortól Height sor hosszan tart 
submatrix(Mx, StartC, Width, StartR, Height, Smx) :-
	sublist(Mx, StartR, Height, Lt),
	cutAll(Lt, StartC, Width, Smx).

% cutAll(Mx, StartR, Height, M) M mátrix Mx olyan részmátrixa, mely annak
% Height sorát tartalmazza sorfolytonosan a StartR. sortól kezdve	
cutAll([], _, _, []).
cutAll([H|T], StartR, Height, [HSmx|TSmx]) :-
	isList(H),
	sublist(H,StartR,Height, HSmx),
	cutAll(T, StartR, Height, TSmx).

% sublist(L, S, N, L1): Az L1 lista az L lista N hosszúságú részlistája az S. elemmel kezdve
sublist(L, S, N, L1) :-
	S1 is S -1,
	drop(L,S1,Ltemp),
	take(Ltemp,N,L1).

% take(L0, N, L): Az L lista az L0 lista N hosszú prefixuma
take([], _, []).
take([_|_], 0, []).
take([H|T], N, [H | Taken]) :-
	N > 0,
	N1 is N-1,
	take(T, N1, Taken).

% drop(L0, N, L): Az L0 lista olyan szuffixuma L, amely az L0 elsõ N elemét
%% nem tartalmazza
drop([], _, []).
drop([H|T], 0, [H|T]).
drop([_|T], N, Dropped) :-
	N > 0,
	N1 is N-1,
	drop(T,N1,Dropped).

% countC(Mx, C, Cc, Acc): Cc = Acc + ahány részre esik Mx mátrix C oszloponként vágva
countC([H|_], C, Cc, 0) :- 
	isList(H),
    countR(H,C,Cc,0).

%countR(Mx, R, Cr, Acc): Cr = Acc + ahány részre esik Mx mátrix R soronként vágva
countR([], _, 0, _).
countR([H|T], R, Cr, 0) :- 
	countR([H|T],R,UjCr,R),
	Cr is UjCr + 1.
countR([_|T], R, Cr, Rem) :-
	Rem > 0,
	UjRem is Rem -1,
	countR(T, R, Cr, UjRem).

% flattenNextLvl(L,Fl): Fl az L lista elemeit taralmazza, mindet kilapítva
flattenNextLvl([],[]).
flattenNextLvl([H0|T0],[H1|T1]):-
	flatten(H0,H1),
	flattenNextLvl(T0,T1).
	

% flatten(L,Fl) : Fl L lista kilapítva
flatten(L, Fl):-
  flatten(L, Fl, []).

% flatten(L, Fl, Acc) Fl az L lista kilapítva és Acc elé fűzve
flatten([], Fl, Fl).
flatten(H, [H|Fl], Fl):-
  \+ isList(H).
flatten([H|T], Fl, Acc):-
  flatten(H, Fl, L1),
  flatten(T, L1, Acc).

% isList(L) L egy tetszőleges lista (annak eldöntésére, hogy L lista-e)
isList([]).
isList([_|_]).

%------------------------------  /KHF1 ------------------------------
