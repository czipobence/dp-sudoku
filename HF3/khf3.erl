-module(khf3).
-author('czipobence@gmail.com').
-vsn('2016-10-28').
%-export([megoldase/2]).
-compile(export_all).

%% @spec khf3:megoldase(SSpec::sspec(), SSol::ssol()) -> B::bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.
megoldase({K,Mx},Sol) -> true.
