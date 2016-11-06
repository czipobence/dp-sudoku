-module(sudoku).
-author('czipobence@gmail.com').
-vsn('2016-11-03').
-export([sudoku/1]).
%-compile(export_all).

%% @type sspec() = {size(), board()}.
%% @type size()  = integer().
%% @type field() = [info()].
%% @type info()  = e | o | s | w | integer().
%% @type board() = [[field()]].

%% @type ssol() = [[integer()]].

%% @spec sudoku:sudoku(SSpec::sspec()) -> SSols::[ssol()].
%% @doc  SSols az SSpec feladványt kielégítő megoldások listája.
sudoku({K,Bo}) -> 
	try solve(K,Bo) of
		Val -> Val
	catch 
		throw:no_solution -> []
	end.


%% @spec sudoku:solve(K::size(),B::board()) -> SSols::[(ssol)]
%% SSols a {K,B} feladványt kielegito megoldasok listaja, ha nincs ilyen
%% megoldas, kivetel dobodik
solve(K,Bo) ->
	BoPP = preprocess(Bo),
	Vals = allowed(K,BoPP),
	solve(K,Vals,Bo).
	
%% @type possib() 	= integer() | integer()
%% @type prow()		= [possib()]
%% @type psol() 	= [prow()]
	
%% @spec sudoku::solve(K::size(), Vals::psol(), Bo::board) ->
%% SSols::[ssol()]
%% SSols a {K,Bo} feladvanyt kielegito megoldasok listaja, ahol minden 
%% mezo erteke vals megfelelo mezoje altal megadott ertekek kozul egy.
solve(K, Vals,Bo) ->
	try
		Temp = process_singles(K,Vals,Bo),
		case shortest_list(Temp) of
			{0,0,0,_} -> [Temp];
			{_,R,C,V} ->
				Inf = nth_matr(Bo,R,C),
				split_by(K,Temp,Bo,R,C,V,Inf,[]) 
		end
	of
		Val -> Val
	catch 
		throw:no_solution -> []
	end.

%% @spec sudoku:split_by(K::size(), Vals::psol(), Bo::board(),
%%       R::integer(),C::integer(),L::[integer()],Acc::[ssol()]) ->
%%       Sol::[ssol()].
%% Sol lista Acc lista osszefuzve a K parameteru, Bo informaciokkal
%% rendelkezo sudoku  Vasl altal megengedett lehetseges ertekek melletti
%% lehetseges megoldasainak listaja, miutan R-C soraban, Inf info
%% mellett Inf cellainfok mellett az L lista altal megengedett infok
%% valamelyiket lekotjuk. 
split_by(_,_,_,_,_,[],_,Acc) -> Acc;
split_by(K,Vals,Bo,R,C,[H|T],Inf,Acc) ->
	try bind_value(K,Vals,R,C,H,Inf) of
		V -> split_by(K,Vals,Bo,R,C,T,Inf, merge(solve(K,V,Bo),Acc))
	catch
		throw:no_solution -> split_by(K,Vals,Bo,R,C,T,Inf, Acc)
	end. 
	
%% @spec sudoku:process_singles(K::size(),Vals::psol(),Bo::board()) ->
%%		 Vals1::psol().
%% Vals1 Vals lehetseges mezoertekek kozul az elso egyedul lehetseges
%% elem lekotese BoPP mezoinfok mellett
process_singles(K,Vals,BoPP) ->
	try process_one_single(Vals, BoPP,1) of 
		{R,C,N,Inf} -> 
			Result = bind_value(K,Vals,R,C,N,Inf),
			process_singles(K,Result,BoPP)
	catch
		throw:no_more_single -> Vals
	end.

%% @spec sudoku:process_one_single(Vals::psol(),Bo::board(), 
%%       R::integer()) -> Proc::psol().
%% Proc Val mezoertekekben egy, az R. sor utan kovetkezo, egyedul allo
%% lehetseges ertek kotese utan, Bo mezoinfok mellett.
process_one_single([],_,_) -> throw(no_more_single);
process_one_single([HV|TV],[HB|TB],R) -> 
	try process_one_single_row(HV,HB,R,1) of
		Result -> Result
	catch
		throw:no_more_single -> process_one_single(TV,TB,R+1)
	end.

%% @spec(L::prow(),B::[field()],R::integer(),C::integer) -> L1::prow().
%% L1 lista L listaban egy egyedul allo lehetseges ertek lekotese	
process_one_single_row([],_,_,_) -> throw(no_more_single);
process_one_single_row([[H]|_], [HB|_],R,C) -> {R,C,H,HB};
process_one_single_row([_|TV], [_|TB],R,C) -> 
	process_one_single_row(TV,TB,R,C+1).

%% legyen R,C,N,I informacio egy lekotendo mezorol: R a sora, 
%% C az oszlopa, N az erteke es a mezore korabban megadott I infok 
%% (amiket persze N kielegit)

%% @spec sudoku:bind_value(K::size(),Vals::psol(),R::integer(),
%%       C::integer(),N::integer,I::[info]) -> Sol::psol().
%% Sol a lehetseges ertekek azutan, hogy K parameteru sudoku Vals 
%% mezoinek lehetseges ertekei kozul egyet a R,C,N,I mezoleiro alapjan
%% lekotunk.
bind_value(K,Vals,R,C,N,Inf) ->
	lists:reverse(bind_value(K,Vals,R,C,N,Inf,1,[])).

%% @spec sudoku:bind_value(K::size(),Vals::[prow()],R::integer(),
%%       C::integer(),N::integer,I::[info], Rc::integer(),
%%       Acc::[prow()]) -> Sol::psol().
%% Sol Acc Rc-1 soru matrix es egy olyan matrix osszefuzve, ami
%% tartalmazza a frissitett lehetseges ertekeket egy R,C,N,I letkotes
%% utan, felteve hogy Vals az eredeti lehetseges ertekek reszmatrixa,
%% az Rc. sortol kezdve
bind_value(_,[],_,_,_,_,_,Acc) -> Acc;
bind_value(K,[H|T],R,C,N,Inf,Rc,Acc) ->
	bind_value(K,T,R,C,N,Inf,Rc+1, 
			[lists:reverse(bind_value_row(K,H,R,C,N,Inf,Rc,1,[]))|Acc]).
	
%% @spec sudoku:bind_value_row(K::size(), CR::[possib()],R::integer(),
%%       C::integer(),N::integer,I::[info], Rc::integer(),
%%       Cc::integer(), Acc::[possib()]) -> SR::prow().
%% SR egy lehetseges megoldassor frissitve egy K parameteru sudokuban,
%% R,C,N,I parameteru lekotes elvegzese utan, amennyiben Acc tartalmazza
%% a sor Cc. eleme elotti ertekek frissiteset CR meg a Cc elemtol kezde
%% a sor elemeit
bind_value_row(_,[],_,_,_,_,_,_,Acc) -> Acc;
bind_value_row(K,[_|T],R,C,V,I,R,C,Acc) -> 
	bind_value_row(K,T,R,C,V,I,R,C+1,[V|Acc]);
bind_value_row(K,[H|T],R,C,V,I,Rc,C,Acc) when Rc == R-1 ->
	GuardS = lists:member(n,I),
	H1 = if 
		GuardS -> 
			if 
				V rem 2 == 0	-> filter_odds(H);
				true 			-> filter_evens(H)
			end;
		true -> H
	end,
	bind_value_row(K,T,R,C,V,I,Rc,C+1,[delete_guess(V,H1)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,Rc,C,Acc) when Rc == R+1 ->
	GuardS = lists:member(s,I),
	H1 = if 
		GuardS -> 
			if 
				V rem 2 == 0	-> filter_odds(H);
				true 			-> filter_evens(H)
			end;
		true -> H
	end,
	bind_value_row(K,T,R,C,V,I,Rc,C+1,[delete_guess(V,H1)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,R,Cc,Acc) when Cc == C+1 ->
	GuardS = lists:member(a,I),
	H1 = if 
		GuardS -> 
			if 
				V rem 2 == 0	-> filter_odds(H);
				true 			-> filter_evens(H)
			end;
		true -> H
	end,
	bind_value_row(K,T,R,C,V,I,R,Cc+1,[delete_guess(V,H1)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,R,Cc,Acc) when Cc == C-1 ->
	GuardS = lists:member(w,I),
	H1 = if 
		GuardS -> 
			if 
				V rem 2 == 0	-> filter_odds(H);
				true 			-> filter_evens(H)
			end;
		true -> H
	end,
	bind_value_row(K,T,R,C,V,I,R,Cc+1,[delete_guess(V,H1)|Acc]);

bind_value_row(K,[H|T],R,C,V,I,R,Cc,Acc) -> 
	bind_value_row(K,T,R,C,V,I,R,Cc+1,[delete_guess(V,H)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,Rc,C,Acc) -> 
	bind_value_row(K,T,R,C,V,I,Rc,C+1,[delete_guess(V,H)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,Rc,Cc,Acc) when 
		(((R - (R-1) rem K) == (Rc - (Rc - 1) rem K)) andalso 
		((C - (C-1) rem K) == (Cc -(Cc-1) rem K))) -> 
	bind_value_row(K,T,R,C,V,I,Rc,Cc+1,[delete_guess(V,H)|Acc]);
bind_value_row(K,[H|T],R,C,V,I,Rc,Cc,Acc) -> 
	bind_value_row(K,T,R,C,V,I,Rc,Cc+1,[H|Acc]).

%% @spec sudoku:delete_guess(V::integer(), L::possib()) -> L1::possib().
%% L1 egy olyan lehetseges mezoerteket leiro objektum, melynek minden
%% eleme megegyezik L-el, csak nem tartalamazza V-t. L1 nem lehet ures
%% lista, ebben az esetben no_solution kivetel dobodik
delete_guess(_, []) -> throw(no_solution);
delete_guess(V, [V]) -> throw(no_solution);
delete_guess(V, [H|T]) -> lists:delete(V,[H|T]);
delete_guess(V, V) -> throw(no_solution);
delete_guess(_,N) -> N.

%% @spec sudoku:preprocess(M::board()) -> M1::board().
%% M1 egy tabla, ami a feldolgozott infokat tartaknazza, kiegeszitve 
%% azokat north (n) es east (a) informaciokkal
preprocess(Bo) -> lists:reverse(preprocess([],Bo,[])).

%% @spec sudoku:preprocess(PreR::[field()],M::board(),Acc::board()) ->
%%		 M1::board().
%% M1 egy tabla, mi M alapjan feldolgozott informaciokat tartalmaz, 
%% felteve hogy M egy olyan tabla utolso nehany sora, melynek elozo sora
%% PreR, es a tabla azon sorainak feldolgozasait, amik nincsenek benne
%% M-ben, azokat Acc tartalmazza
preprocess(_,[],Acc) -> Acc;
preprocess(PreR,[CurrR|T], Acc) ->
	preprocess(CurrR,T,[lists:reverse(
				preprocess_row(PreR,CurrR,[])) | Acc]).
	
%% @spec sudoku::preprocess_row(PreR::[field()],CurrR::[field()],	
%%       Acc::[field()]) -> PR::[field()].
%% PR egy olyan sor feldolgozasa, melynek utolso nehany eleme CurrR, az
%% felette levo sor elemei rendre PreR elemei, mig a sor CurrR altal nem
%% tartalmazott elemeit Acc tartalmazza
preprocess_row(_,[],Acc) -> Acc;
preprocess_row([], [CurrI], Acc) -> [CurrI | Acc];
preprocess_row([], [CurrI,NextI|CurrT], Acc) -> 
	GuardW = lists:member(w,NextI),
	if 	GuardW 	-> 
			preprocess_row([], [NextI | CurrT], [ [a | CurrI] | Acc ]);
		true 	-> 
			preprocess_row([], [NextI | CurrT], [CurrI | Acc])
	end;
preprocess_row([PreI], [CurrI], Acc) -> 
	Guard = lists:member(s,PreI),
	if 	Guard -> [[n | CurrI]| Acc];
		true -> [CurrI | Acc]
	end;
preprocess_row([PreI|PreT], [CurrI,NextI|CurrT], Acc) ->
	GuardS = lists:member(s,PreI),
	GuardW = lists:member(w,NextI),
	CurrI1 = if	
		GuardS -> [n | CurrI];
		true -> CurrI
	end,
	if 
		GuardW ->
			preprocess_row(PreT, [NextI|CurrT], [ [a | CurrI1] | Acc ]);
		true -> 
			preprocess_row(PreT, [NextI|CurrT], [CurrI1 | Acc])
	end.

%% @spec sudoku:allowed(K::integer(), M::board()) ->
%%       Vals::[[[integer()]]].
%% Vals a Bo infoi altal megengedett mezoertekek matrixa
allowed(K,Bo) -> [[ parse_ertek(ertekek(K,Bo,R,C))
						|| C <- lists:seq(1,K*K)]
					|| R <- lists:seq(1,K*K) ].

%% @spec sudoku::parse_ertek(L::any()) -> L1::any().
%% L1 = L ha L nem ures lista, kulonben kivetel dobodik
parse_ertek([]) -> throw(no_solution);
parse_ertek(L) -> L.

%% @spec sudoku:ertekek(K::integer(), M::board(), R::integer(), 
%%       C::integer()) -> Vals::[integer()].
%% Vals a {K,M} specifikációval megadott Sudoku-feladvány R-C
%% koordinátájú mezőjében megengedett értékek listája.
ertekek(K, M, R,C) -> 
	Val = nth_matr(M,R,C),
	Mx = set_nth_matr(M,R,C,[]),
	AllNum = lists:seq(1,K*K),
	L = merge(flatten(
		get_row(Mx,R)), 
		merge(
			flatten(get_col(Mx,C)), 
			flatten(submatrix(Mx, top(R,K), top(C,K) ,K,K)
		)
	)),
	Allowed = lists:foldr(
		fun(X,Acc)->intersection(X,Acc) end, AllNum, 
		lists:map(fun(X) -> allowed_by(X,K) end,L)
	),
	Limited = lists:foldr(
		fun(X,Acc)->intersection(X,Acc) end, AllNum, 
		lists:map(fun(X) -> limited_to(X,K) end,Val)
	),
	intersection(Allowed,Limited).


%% @spec sudoku:limited_to(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru sudoku adott mezojeben lehetnek
limited_to(V,K) when is_number(V) -> 
	case V < K*K+1 of
	true -> [V];
	false -> []
	end;
limited_to(o,K) -> lists:reverse(secs_till(K*K,1,[]));
limited_to(e,K) -> lists:reverse(secs_till(K*K,2,[]));
limited_to(_,K) -> lists:seq(1,K*K).

%% @spec sudoku:allowed_by(V::field(), K::integer()) -> L::[integer()].
%% L lista azon szamok listaja, amelyek V info mellett egy K parameteru 
%% sudokuban az adott mezovel egy szegmensben (K*K negyzet, sor, oszlop)
%% engedelyezettek
allowed_by(V,K)when is_number(V) -> lists:reverse(miss(V,K*K));
allowed_by(_,K) -> lists:seq(1,K*K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sudoku related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec sudoku:top(R::integer(), K::integer()) -> Top::integer().
%% Top a legnagyobb, R-nel kisebb K-val oszthato poz. eg. szam
top(R,K) -> top(R,K,1).
top(R,K,Acc) ->
	case Acc + K > R of
	true -> Acc;
	false -> top(R,K,Acc+K)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matrixok
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @type matrix() = [[any()]].

%% @spec sudoku:get_row(Mx::matrix(), Row::integer()) -> L::[any()].
%% Az L lista Mx matrix Row. oszlopa
get_row(Mx, Row) -> lists:nth(Row, Mx).

%% @spec sudoku:get_col(Mx::matrix(), Col::integer()) -> L::[any()].
%% Az L lista Mx matrix Col. oszlopanak elemei (oszlop helyett listava lapitva)
get_col(Mx, Col) -> [lists:nth(Col,L) || L <- Mx].

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @spec sudoku:nth_matr(M::matrix(), Row::integer(),Col::integer()) ->
%%		 T::any().
%% T M matrix Row. soraban es Col. oszlopaban talalhato eleme
nth_matr(M, Row, Col) -> lists:nth(Col, lists:nth(Row, M)).

%% @spec sudoku:set_nth_matr(M::matrix(), Row::integer(),Col::integer(),
%%       To::any()) -> Mx::matrix().
%% Mx olyan matrix, ami megegyezik M-el, kiveve Row soranak Col oszlopa, ahol az elem To
set_nth_matr(M, Row,Col,To) -> [
		[set_nth_proc_element(nth_matr(M,R,C), R, C, Row, Col, To) 
						|| C <- lists:seq(1,length(hd(M)))] 
				|| R <- lists:seq(1,length(M))].

%% @spec sudoku:set_nth_proc_element(E::any(),R::integer(),C::integer(),
%%       Row::integer(), Col::integer(),To::any()) -> E1::matrix().
%% E1 az E, kiveve ha Row = R es Col = C, mert akkor To
set_nth_proc_element(_,R,C,R,C,To) -> To;
set_nth_proc_element(E,_,_,_,_,_) -> E.

%% @spec sudoku:submatrix(Mx::matrix(), StartX::integer(),
%%       StartY::integer(),Width::subRows(),Height::subCols()) -> 
%%		 M::matrix().
%% M mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartX oszloptól Width oszlop szélesen,
%% és a StartY sortól Height sor hosszan tart 									
submatrix(Mx, StartX, StartY, Width, Height) ->
	[lists:sublist(L, StartY, Height) 
		|| L <- lists:sublist(Mx, StartX, Width)].

%% @type sl() = {integer(),integer(),integer(),possib()}

%% @spec sudoku::shortest_list(Mx::psol()) -> SL::sl().
%% ahol SL Mx-ben talalhato listak kozul a legrovidebb meretenek, sor es
%% oszlop koordintatajanak valamit ertekenek negyese. Ha Mx nem 
%% tartalmaz listat, akkor SL={0,0,0,[]}
shortest_list(Mx) -> shortest_list(Mx,1,{0,0,0,[]}).

%% @spec sudoku::shortest_list(Mx::psol(),R::integer(),Cnt::sl()) ->
%%		 SL::sl().
%% SL egy olyan psol() legrovidebb listaja, melynek elso R-1 soraban a
%% legrovidebb lista Cnt, a tobbi sora pedig Mx
shortest_list([], _, Cnt) -> Cnt;
shortest_list([H|T], R, Cnt) -> 
	shortest_list(T, R+1, shortest_list_row(H,R,1,Cnt)).
	
%% @spec sudoku::shortest_list_row(Row::prow(),R::integer(),
%%	     C::integer(),Cnt::sl()) -> SL::sl().
%% SL egy olyan prow() legrovidebb listajat leiro negyes, amely egy 
%% psol() R. soraban talalhato, es amely sor elso C-1 eleme kozul a 
%% legrovidebb listat leiro negyes Cnt, a tobbi elemet pedig Row
%% tartalmazza 
shortest_list_row([],_,_,Cnt) -> Cnt;
shortest_list_row([H|T],R,C, {Min,MinR,MinC,Val}) ->
	case H of
		[] ->
			throw(no_solution);
		[_|_] ->
			if
				((length(H) < Min) or (Min == 0)) -> 
					shortest_list_row(T,R,C+1, {length(H),R,C,H});
				true ->
					shortest_list_row(T,R,C+1,{Min,MinR,MinC,Val})
			end;
		_ -> shortest_list_row(T,R,C+1,{Min,MinR,MinC,Val})
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Listakezeles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%% @spec sudoku::filter_odds(L::possib()) -> L1::possib().
%% L1 L listabol a paratlan szamok, vagy L1=L ha L egy paratlan szam.
%% Ha L egy paros szam, no_solution kivetel dobodik
filter_odds([]) -> [];
filter_odds([H|T]) ->
	if
		(H rem 2) == 0 -> filter_odds(T);
		true -> [H| filter_odds(T)]
	end;
filter_odds(N) ->
	if
		N rem 2 == 0 -> 
			throw(no_solution);
		true ->
			N
	end.

%% @spec sudoku:filter_evens(L::possib()) -> L1::possib().
%% L1 L listabol a paros szamok, vagy L1=L ha L egy paros szam.
%% Ha L egy paratlan szam, no_solution kivetel dobodik
filter_evens([]) -> [];
filter_evens([H|T]) ->
	if 
		(H rem 2 == 1) -> filter_evens(T);
		true -> [H | filter_evens(T)]
	end;
filter_evens(N) ->
	if
		N rem 2 == 1 -> 
			throw(no_solution);
		true ->
			N
	end.

%% @spec sudoku:intersection(L1::[any()], L2::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete
intersection(L1,L2) -> lists:reverse(intersection(L1,L2,[])).

%% @spec sudoku:intersection(L1::[any()], L2::[any()], Coll::[any()]) ->
%%       L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete osszefuzve Coll-al
intersection([],_,Coll) -> Coll;
intersection(_, [], Coll) -> Coll;
intersection([H|T1], [H|T2], Coll) -> intersection(T1,T2,[H|Coll]);
intersection([H1|T1], [H2|T2], Coll) when H1 < H2 -> 
		intersection(T1, [H2|T2], Coll);
intersection([H1|T1], [_|T2], Coll) -> 
		intersection([H1|T1], T2, Coll).

%% @spec sudoku:secs_till(N::integer(), I::integer(), L::[integer()]) ->
%%       L1::[integer()].
%% L lista I tol kezdve N-ig minden masodik szam listaja, megforditva L 
%% ele fuzve
secs_till(N,I,L) ->
	case I < N+1 of
	true -> secs_till(N,I+2,[I|L]); 
	false -> L
	end.

%% @spec sudoku:miss(V::integer(), N::integer()) -> L::[integer()].
%% L lista N tol 1-ig a szamok, V-t kihagyva
miss(V,N) -> miss(V,N,1,[]).

%% @spec sudoku:miss(V::integer(), N::integer(), Curr:integer(), 
%%       L:[integer()]) -> L1::[integer()].
%% L1 lista N-tol Curr-ig a szamok, V-t kihagyva, L ele fuzve
miss(N,N,N,L) -> L;
miss(_,N,N,L) -> [N|L];
miss(V,N,V,L) -> miss(V,N,V+1,L);
miss(V,N,I,L) -> miss(V,N,I+1, [I|L]).
	
%% @spec sudoku:merge(L0::[any()], L1::[any()]) -> L::[any()].
%% L lista L0 es L1 osszefuzve
merge([],L) -> L;
merge([H|T], L) -> merge(T, [H|L]).

%% @spec sudoku:flatten(Mx::matrix()) -> LL::[any()].
%% Az LL lista az Mx mátrix kilapítva
flatten(Mx) -> flatten(Mx, []).


%% @spec sudoku:flatten(Mx::matrix(), L::[term()]) -> L1::[term()].
%% Az L1 lista az Mx mátrix kilapítva és L lista mögéfûzve
flatten([],L) -> L;
flatten([H|T], L) when is_list(H) -> flatten(H, flatten(T,L));
flatten([H|T], L) -> [H | flatten(T,L)].
