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
