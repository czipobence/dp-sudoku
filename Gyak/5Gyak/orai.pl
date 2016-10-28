%1.feladat

insert_ord([], Elem, [Elem]).
insert_ord([H|T], Elem, L) :-
		( Elem < H ->	L = [Elem | [H|T]]
		; Elem =:= H -> L = [H|T]
		; Elem > H -> insert_ord(T,Elem,L1),
		L = [H | L1]
		).

%2. feladat

draw([],[]). 
draw(G, [P-Q|T]) :-
	select_edge(P,Q,G,G1),
	( G1 = [] -> T = []
	; T = [Q-_|_],
	  draw(G1,T)
	).

%G gráfból elhagyjuk P-Q élet és így kapjuk G1et
select_edge(P,Q,G,G1) :-
	select(E,G,G1),
	(
	E = P-Q;
	E = Q-P
	).
select(E, [E|T], T).
select(E, [H|T], [H|L]) :-
	select(E,T,L).
	

%5. feladat
pl_kezdetu([A,A,A|T],N,Rem) :-
	pl_kezdetu([A,A|T],N1,Rem),
	N is N1 + 1.
pl_kezdetu([A,A,B|T],2,[B|T]) :-
	A \= B.
pl_kezdetu([A,A],2,[]).

%6. feladat
plato(L,H,X) :-
	L = [H1|T],	
	(pl_kezdetu([H1|T],Len0,M0) ->
	( X = Len0, H = H1
	; plato(M0,H,X)
	)
	;	
	plato(T,H,X)	
	).
