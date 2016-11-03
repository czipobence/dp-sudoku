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
