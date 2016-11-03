-module(sudoku).
-author('czipobence@gmail.com').
-vsn('2016-11-03').
%-export([sudoku/1]).
-compile(export_all).

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

solve(K,Bo) ->
	BoPP = preprocess(K,Bo),
	Vals = allowed(K,BoPP),
	apply_sw(K,Vals,BoPP).

apply_sw(K,Vals,Bo) -> 
	[[ apply_sw(K,Vals,Bo,R,C) 
		|| C <- lists:seq(1,K*K) ]
		|| R <- lists:seq(1,K*K) ].
	
apply_sw(K,Vals,Bo,R,C) ->
	Restr = verify_to_sw_infos(K,Vals,Bo,R,C,nth_matr(Vals,R,C),nth_matr(Bo,R,C)),
	apply_sw_neighbors(K,Vals,Bo,R,C,Restr).
	
apply_sw_neighbors(K,Value,Bo,R,C,Restr) ->
	apply_w_next(K,Value,Bo,R,C,apply_s_above(Value,Bo,R,C, Restr)).
	
apply_s_above(_,_,1,_,Restr) -> Restr;
apply_s_above(Value,Bo,R,C,Restr) -> 
	Grd = lists:member(s,nth_matr(Bo,R-1,C)),
	if  Grd
		-> case nth_matr(Value,R-1,C) of
			N when is_number(N) -> 
				if is_list(Restr) ->
					if (N rem 2 == 0) -> filter_odds(Restr);
					true -> filter_evens(Restr)
					end;
				true -> 
					if (N rem 2 == Restr rem 2) -> throw (no_solution);
					true -> Restr
					end
				end;
			_ -> Restr
		end;
		true -> Restr 
	end.
	
apply_w_next(K,_,_,_,N,Restr) when N == K*K -> Restr;
apply_w_next(_,Value,Bo,R,C,Restr) -> 
	Grd = lists:member(w,nth_matr(Bo,R,C+1)),
	if Grd
		-> case nth_matr(Value,R,C+1) of
			N when is_number(N) -> 
				if is_list(Restr) ->
					if (N rem 2 == 0) -> filter_odds(Restr);
					true -> filter_evens(Restr)
					end;
				true -> 
					if (N rem 2 == Restr rem 2) -> throw (no_solution);
					true -> Restr
					end
				end;
			_ -> Restr
		end;
		true -> Restr 
	end.
	
verify_to_sw_infos(_,_,_,_,_,PV,[]) ->
	PV;
verify_to_sw_infos(K,Vals,Bo,R,C,PV,[s|T]) ->
	if is_list(PV) ->
		case nth_matr(Vals,R+1,C) of
		N when is_number(N) ->
			if (N rem 2 == 0) -> verify_to_sw_infos(K,Vals,Bo,R,C,filter_odds(PV),T);
			true -> verify_to_sw_infos(K,Vals,Bo,R,C,filter_evens(PV),T)
			end;
		_ -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
		end;
	true ->
		case nth_matr(Vals,R+1,C) of
		N when is_number(N) -> 
			if (PV rem 2) == (N rem 2) -> throw(no_solution);
			true -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
			end;
		_ -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
		end
	end;
verify_to_sw_infos(K,Vals,Bo,R,C,PV,[w|T]) ->
	if is_list(PV) ->		
		case nth_matr(Vals,R,C-1) of
		N when is_number(N) ->
			if (N rem 2 == 0) -> verify_to_sw_infos(K,Vals,Bo,R,C,filter_odds(PV),T);
			true -> verify_to_sw_infos(K,Vals,Bo,R,C,filter_evens(PV),T)
			end;
		_ -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
		end;
	true ->
		case nth_matr(Vals,R,C-1) of
		N when is_number(N) -> 
			if (PV rem 2) == (N rem 2) -> throw(no_solution);
			true -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
			end;
		_ -> verify_to_sw_infos(K,Vals,Bo,R,C,PV,T)
		end
	end;
verify_to_sw_infos(K,Vals,Bo,R,C,PV,[_|T]) ->
	verify_to_sw_infos(K,Vals,Bo,R,C,PV,T).
	


%% @spec sudoku:preprocess(K::integer(), M::board()) -> M1::board()
%%	 M1 egy tabla, ami feldolgozza az infokat
preprocess(_,Bo) -> Bo. %TODO

%% @spec sudoku:allowed(K::integer(), M::board()) -> Vals::[[[integer()]]]
%%	 Vals a Bo infoi altal megengedett mezoertekek matrixa
allowed(K,Bo) -> [[ parse_ertek(ertekek(K,Bo,R,C))
				  || C <- lists:seq(1,K*K)]
				  || R <- lists:seq(1,K*K) ].

parse_ertek([]) -> throw(no_solution);
parse_ertek([H]) -> H;
parse_ertek(L) -> L.

%% @spec sudoku:ertekek(K::integer(), M::board(), R::integer(), C::integer()) -> Vals::[integer()]
%%   Vals a {K,M} specifikációval megadott Sudoku-feladvány R-C
%%   koordinátájú mezőjében megengedett értékek listája.
ertekek(K, M, R,C) -> 
	Val = nth_matr(M,R,C),
	Mx = set_nth_matr(M,R,C,[]),
	AllNum = lists:seq(1,K*K),
	L = merge(flatten(get_row(Mx,R)), merge(flatten(get_col(Mx,C)), flatten(submatrix(Mx, top(R,K), top(C,K) ,K,K)))),
	Allowed = lists:foldr(fun(X,Acc)->intersection(X,Acc) end, AllNum, lists:map(fun(X) -> allowed_by(X,K) end,L)),
	Limited = lists:foldr(fun(X,Acc)->intersection(X,Acc) end, AllNum, lists:map(fun(X) -> limited_to(X,K) end,Val)),
	intersection(Allowed,Limited).

%% @type info()  = e | o | s | w | integer().
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

%% @spec sudoku:get_row(Mx::matrix(), Row::integer()) -> L::[any()].
%% Az L lista Mx matrix Row. oszlopa
get_row(Mx, Row) -> lists:nth(Row, Mx).

%% @spec sudoku:get_col(Mx::matrix(), Col::integer()) -> L::[any()].
%% Az L lista Mx matrix Col. oszlopanak elemei (oszlop helyett listava lapitva)
get_col(Mx, Col) -> [lists:nth(Col,L) || L <- Mx].

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @spec sudoku:nth_matr(M::matrix(), Row::integer(),Col::integer()) -> T::any().
%% T M matrix Row. soraban es Col. oszlopaban talalhato eleme
nth_matr(M, Row, Col) -> lists:nth(Col, lists:nth(Row, M)).

%% @spec sudoku:set_nth_matr(M::matrix(), Row::integer(),Col::integer(),To::any()) -> Mx::matrix().
%% Mx olyan matrix, ami megegyezik M-el, kiveve Row soranak Col oszlopa, ahol az elem To
set_nth_matr(M, Row,Col,To) -> [
		[set_nth_proc_element(nth_matr(M,R,C), R, C, Row, Col, To) 
						|| C <- lists:seq(1,length(hd(M)))] 
				|| R <- lists:seq(1,length(M))].

%% @spec sudoku:set_nth_proc_element(E::any(), R::integer(),C::integer(), Row::integer(), Col::integer(),To::any()) -> E1::matrix().
%% E1 az E, kiveve ha Row = R es Col = C, mert akkor To
set_nth_proc_element(_,R,C,R,C,To) -> To;
set_nth_proc_element(E,_,_,_,_,_) -> E.

%% @spec sudoku:submatrix(Mx::matrix(), StartX::integer(), StartY::integer(),Width::subRows(), Height::subCols()) -> M::matrix()
%% M mátrix az Mx mátrix olyan sor és oszlopfolytonos részmátrixa,
%% ami a StartX oszloptól Width oszlop szélesen,
%% és a StartY sortól Height sor hosszan tart 									
submatrix(Mx, StartX, StartY, Width, Height) ->
	[lists:sublist(L, StartY, Height) || L <- lists:sublist(Mx, StartX, Width)].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Listakezeles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
filter_odds([]) -> [];
filter_odds([H|T]) ->
	if
		(H rem 2) == 0 -> filter_odds(T);
		true -> [H| filter_odds(T)]
	end.

filter_evens([]) -> [];
filter_evens([H|T]) ->
	if 
		(H rem 2 == 1) -> filter_evens(T);
		true -> [H | filter_evens(T)]
	end.

is_even(V) -> V rem 2 == 0.

%% @spec sudoku:intersection(L1::[any()], L2::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete
intersection(L1,L2) -> lists:reverse(intersection(L1,L2,[])).

%% @spec sudoku:intersection(L1::[any()], L2::[any()], Coll::[any()]) -> L::[any()].
%% L L1 L2 szigoruan monoton novekvo listak metszete osszefuzve Coll-al
intersection([],_,Coll) -> Coll;
intersection(_, [], Coll) -> Coll;
intersection([H|T1], [H|T2], Coll) -> intersection(T1,T2,[H|Coll]);
intersection([H1|T1], [H2|T2], Coll) when H1 < H2 -> 
		intersection(T1, [H2|T2], Coll);
intersection([H1|T1], [_|T2], Coll) -> 
		intersection([H1|T1], T2, Coll).

%% @spec sudoku:secs_till(N::integer(), I::integer(), L::[integer()]) -> L1::[integer()].
%% L lista I tol kezdve N-ig minden masodik szam listaja, megforditva L ele fuzva
secs_till(N,I,L) ->
	case I < N+1 of
	true -> secs_till(N,I+2,[I|L]); 
	false -> L
	end.

%% @spec sudoku:miss(V::integer(), N::integer()) -> L::[integer()].
%% L lista N tol 1-ig a szamok, V-t kihagyva
miss(V,N) -> miss(V,N,1,[]).

%% @spec sudoku:miss(V::integer(), N::integer(), Curr:integer(), L:[integer()]) -> L1::[integer()].
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
