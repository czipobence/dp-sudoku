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
sudoku({K,Bo}) -> Bo.
