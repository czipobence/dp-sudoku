-module(orai).
-author('czipobence@gmail.com').
-vsn('2016-11-03').
-compile(export_all).

fa_noveltje(level) -> level;
fa_noveltje({N,F1,F2}) -> {N+1,fa_noveltje(F1),fa_noveltje(F2)}.

fa_tukorkepe(level) -> level;
fa_tukorkepe({N,R,L}) -> {N,fa_tukorkepe(L),fa_tukorkepe(R)}.
