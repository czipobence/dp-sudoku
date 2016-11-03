-module(orai).
-author('czipobence@gmail.com').
-vsn('2016-11-03').
-compile(export_all).

fa_noveltje(level) -> level;
fa_noveltje({N,F1,F2}) -> {N+1,fa_noveltje(F1),fa_noveltje(F2)}.

fa_tukorkepe(level) -> level;
fa_tukorkepe({N,L,R}) -> {N,fa_tukorkepe(R),fa_tukorkepe(L)}.

fa_balerteke(level) -> error;
fa_balerteke({N,level,_}) -> {ok, N};
fa_balerteke({_,L,_}) -> fa_balerteke(L).

fa_jobberteke(level) -> error;
fa_jobberteke({N,_,level}) -> {ok, N};
fa_jobberteke({_,_,R}) -> fa_jobberteke(R).

rendezett_fa(level) -> true;
rendezett_fa({_,level,level}) -> true;
rendezett_fa({N,L,level}) ->
	{ok, JE} = fa_jobberteke(L), 
	rendezett_fa(L) and JE < N;
rendezett_fa({N,level,R}) ->
	{ok, BE} = fa_balerteke(R),
	rendezett_fa(R) and BE > N;
rendezett_fa({N,L,R}) ->
	{ok, BE} = fa_balerteke(R),
	{ok, JE} = fa_jobberteke(L),
	(JE < N) andalso (BE > N) andalso 
	rendezett_fa(L) andalso rendezett_fa(R).
	
tartalmaz(_,level) -> false;
tartalmaz(Mit,{Mit,_,_}) -> true;
tartalmaz(Mit,{_,L,R}) -> tartalmaz(Mit,L) or tartalmaz(Mit,R).

elofordul(_,level) -> 0;
elofordul(Mi,{Mi,L,R}) -> elofordul(Mi,L) + elofordul(Mi,R) + 1;
elofordul(Mi,{_,L,R}) -> elofordul(Mi,L) + elofordul(Mi,R).

utak(F) -> utak(F, []).

utak(level, _) -> [];
utak({N,L,R}, E) -> [{N, lists:reverse(E)} | utak(L, [N | E] )] ++ utak(R,[N|E]). 

cutak(X,F) -> [ {E,L} || {E,L} <- utak(F), E == X ]. 

%TODO 8b, 9

hanyados({tort, _, 0}) ->
	error(badarith);
hanyados({tort, Sz, N}) when is_integer(Sz), is_integer(N) ->
	Sz/N;
hanyados({tort,_,_}) -> error(function_clause).

hanyadosinf(T) ->
	try hanyados(T) of
		V -> V
	catch
		error:badarith -> inf
	end.
	
%lustalista
infseq(N,D) ->
	[N | fun() -> infseq(N+D,D) end].
	
nth(1,[H|_]) -> H;
nth(N,[_|T]) ->
	nth(N-1,T()).
	
%lustamap
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|fun() -> map(F, T()) end].

%bevezeto fac
fac(0) -> 1;
fac(N) -> N * fac(N-1).

invfacs() ->
	map(fun(X) -> 1 / fac(X) end, infseq(0,1)).
