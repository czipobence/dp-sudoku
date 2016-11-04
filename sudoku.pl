% :- type sspec ---> s(size, board).
% :- type size  == int.
% :- type field == list(info).
% :- type info ---> e; o; s; w; v(int).
% :- type board == list(list(field)).

% :- type ssol == list(list(int)).

% sudoku(SSpec, SSol):
% SSol az SSpec feladványt kielégítő megoldás.
% :- pred sudoku(sspec::in, ssol::out).
sudoku(s(K,Bo),Sol) :- sudoku(K,Bo,1,Sol).

sudoku(K,_,R,[]) :- R is K*K + 1.
sudoku(K,Bo,R,[H|T]) :-
	R < K * K + 1,
	sudoku_row(K,Bo,R,1,H),
	R1 is R + 1,
	sudoku(K,Bo,R1,T).

sudoku_row(K,_,_,C,[]) :- C is K * K +1.
sudoku_row(K,Bo,R,C,[H|T]) :-
	C < K * K +1,
	ertekek(s(K,Bo),R-C,H),
	C1 is C + 1,
	sudoku_row(K,Bo,R,C1,T).


% :- type col  == int.
% :- type row  == int.
% :- type coords -->row-col.
% :- pred ertekek(sspec::in, coords::in, list(int)::out).
% ertekek(SSpec, R_C, Vals): 
% Egy érték pontosan akkor szerepel a Vals listában, ha:
%    (a) 1..k*k közötti egész, ahol k az SSpec feladvány cellamérete,
%    (b) teljesíti az adott mezőre vonatkozó szám- és paritási infók
%        által előírt megszorításokat, továbbá
%    (c) különbözik az adott mezőt tartalmazó sor, oszlop és cella többi
%        mezőjében szereplő száminfóktól, 
% ahol
%    SSpec az sspec típusspecifikációnak megfelelő Sudoku-feladvány,
%    R_C az adott feladvány egy mezőjének (sor-oszlop formában megadott) koordinátája,
%    Vals list(int) típusú mezőértéklista, az SSpec feladvány R_C koordinátájú
%         mezőjében megengedett értékek listája. 
ertekek(s(K,Mx), R-C, Vals) :-
	findall(N, ertekek(Mx,R,C,K,N), Vals).

% ertekek (Mx,R,C,K,N)
% egy olyan szam, ami Mx K parameteru sudokuban allhat R,C helyen
ertekek(Mx,R,C,K,N) :-
	mx_item(Mx,R,C,Info),
	remove_mx_element(Mx,R,C,M1),
	list_nth(M1,R,Sor),
	findall(Temp,in_col(M1,C,Temp),Oszlop),
	negyzet(M1,R,C,K,Sq),
	limited_to(Info,K,N),
	allowed_by(Sor,K,N),
	allowed_by(Oszlop,K,N),
	allowed_by(Sq,K,N).

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

%in_col(Mx,N,Item)
%Item megtalalhato Mx matrix N. oszlopaban (1tol kezdve)
in_col([H|T],N,Item) :-
	list_nth(H,N,Item);
	in_col(T,N,Item).

%negyzet(Mx,R,C,K,Sq)
%Sq Mx, K parameteru sudoku R. soraban es C. oszlopaban talalhato elemet bennfoglalo negyzet
negyzet(Mx,R,C,K,Sq) :-
	SR is R - ((R - 1) mod K),
	SC is C - ((C - 1) mod K),
	submatrix(Mx,SR,SC,K,K,Sq).

%remove_mx_element(Mx,R,C,M1)
%M1 Mx matrix, aminek R. soranak C. oszlopaban levo elemet []-re csereltek
remove_mx_element([H|T1],R,C,[H|T2]) :-
	R > 1,
	R1 is R-1,
	remove_mx_element(T1,R1,C,T2).
remove_mx_element([H1|T],1,C,[H2|T]):-
	remove_list_element(H1,C,H2).

%remove_list_element(L,N,L1)
%L1 L lista N. elemenek []-re cserelesevel kapott lista
remove_list_element([H|T1],C,[H|T2]):-
	C > 1,
	C1 is C-1,
	remove_list_element(T1,C1,T2).
remove_list_element([_|T],1,[[]|T]).

%limited_to(V,K,N)
% N olyan szám, amely V info mellett egy K parameteru sudoku adott mezojeben lehet
limited_to([H|T],K,N) :-
	limited_to(H,K,N),
	limited_to(T,K,N).
limited_to(v(V),K,V) :- 
	V < K*K+1.
limited_to(V,K,N) :-
	Top is K*K,
	bet(1,Top,N),
	( V = o -> odd(N)
	; V = e -> even(N)
	; (V = s; V = w; V = [])
	).

%allowed_by(V,K,N)
%N olyan szam ami egy K parameteru sudokuban V infoval azonos szegmensben
%azaz sorban, oszlopban vagy negyzetben, elofordulhat.
allowed_by([H|T],K,N) :-
	allowed_by(H,K,N),
	allowed_by(T,K,N).
allowed_by(V,K,N) :-
	Top is K*K,
	bet(1,Top,N),
	( V = v(Val) -> N \= Val;
	 (V = s; V=e; V=w; V=o; V=[])
	).

%even(N)
%N paros szam
even(N):-
	0 is N mod 2.
%odd(N)
%N paratlan szam
odd(N):-
	1 is N mod 2.

%bet(From,To,N)
%N From es To kozotti szam
bet(From,To,N) :-
	From =< To, N = From.
bet(From,To,N) :-
	From < To,
	From1 is From + 1,
	bet(From1,To,N).


% submatrix(Mx, StartR, StartC, Height, Width, Smx):
%% Smx mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartC oszloptól Width oszlop szélesen,
%% és a StartR sortól Height sor hosszan tart 
submatrix(Mx, StartR, StartC, Height, Width, Smx) :-
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

% isList(L) L egy tetszőleges lista (annak eldöntésére, hogy L lista-e)
isList([]).
isList([_|_]).
