%1.feladat

seq(N,N,[N]).
seq(N,M,[N|T]) :-
	N1 is N+1,
	N < M,
	seq(N1,M,T).


%2. feladat

max(N,X) :- N > 0, X = N.
max(N,X) :- N > 1, N1 is N-1, max(N1, X).
 
%3.feladat

hatv(_,0,1).
hatv(A,E,H) :-
	E > 0, 
	E1 is E-1, 
	hatv(A,E1,H1),
	H is H1*A.
	
%4. feladat

fa_pontszama(leaf(_), 1).
fa_pontszama(node(L,R), S):-
	fa_pontszama(L,S1),
	fa_pontszama(R,S2),
	S is S1 + S2.

%5. feladat
fa_noveltje(leaf(V), leaf(V1)) :- 
	V1 is V+1.
fa_noveltje(node(L,R), node(L1, R1)):-
	fa_noveltje(L,L1),
	fa_noveltje(R,R1).

%6. feladat

lista_hossza([],0).
lista_hossza([_|T], V) :-
	lista_hossza(T, V1),
	V is V1 + 1.

%7. feladat

lista_noveltje([],[]).
lista_noveltje([H|T], [H1|T1]) :-
	lista_noveltje(T,T1),
	H1 is H + 1.

%8. feladat

lista_utolso_eleme([V], V).
lista_utolso_eleme([_|T], V) :- 
	lista_utolso_eleme(T,V).

%9. feladat

fa_levelerteke(leaf(V), V).
fa_levelerteke(node(L,R), V) :-
	fa_levelerteke(L,V);
	fa_levelerteke(R,V).

%10. feladat

fa_reszfaja(leaf(V), leaf(V)).
fa_reszfaja(node(L, R), node(L,R)).
fa_reszfaja(node(L, _), V) :- fa_reszfaja(L, V).
fa_reszfaja(node(_, R), V) :- fa_reszfaja(R, V).

%11. feladat

lista_prefixuma([H|T1], [H|T2]) :- lista_prefixuma(T1, T2).
lista_prefixuma(_,[]).
