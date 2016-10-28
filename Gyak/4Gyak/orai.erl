-module(orai).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seq(F,T) when F < T-> my_seq(F,T,[]).

my_seq(F,F,L) -> [F |L ];
my_seq(F,T,L) when F < T-> my_seq(F, T-1, [T|L]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
zip([], L) -> L;
zip(L, []) -> L; 
zip([H1|T1], [H2|T2]) -> [ {H1, H2} | zip(T1,T2)].

unzip([]) -> {[],[]};
unzip([{H1, H2} | T]) -> {R1, R2} = unzip(T), {[H1 | R1], [H2|R2]}. 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
flatten(L) -> flatten(L, []).	
	
flatten([],L) -> L;
flatten([H|T], L) when is_list(H) -> flatten(H, flatten(T,L));
flatten([H|T], L) -> [H | flatten(T,L)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

submatrix(M, StartCol, StartRow, ColCount, RowCount) ->
	[lists:sublist(X, StartCol, ColCount) || X <- lists:sublist(M, StartRow, RowCount)].
	
submatrix_2(M, StartCol, StartRow, ColCount, RowCount) ->
	[
		[lists:nth(Col, lists:nth(Row, M)) || 
				Col <- lists:seq(StartCol, StartCol + ColCount -1)] ||
			Row <- lists:seq(StartRow, StartRow + RowCount-1)].
	
kozepe(M) ->
	Rows = length(M),
	Cols = length(hd(M)),
	submatrix(M, half(half(Cols))+1, half(half(Cols))+1, half(Cols), half(Rows)).

half(I) -> half(I,0).
half(0,J) -> J;
half(1,J) -> J;
half(I,J) when I>1 -> half(I-2, J+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

laposkozepe(M) -> flatten(kozepe(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pivot(M,R,C) -> 	[
		[lists:nth(Col, lists:nth(Row, M)) || 
				Col <- lists:seq(1, length(hd(M))), Col /= C] ||
			Row <- lists:seq(1, length(M)), Row /= R].
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_different([]) -> true;
all_different([H|T]) -> 
	case lists:member(H,T) of
	false -> all_different(T);
	true -> false
	end.

all_different_b(L) -> A = length(lists:usort(L)),
					B = length(L),
					A == B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 8. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

van2eleme([]) -> false;
van2eleme([_]) -> false;
van2eleme(_) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9. Feladat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

duplak([H|T]) -> duplak(T, H).

duplak([], _) -> [];
duplak([H|T], H) -> [H| duplak(T,H)];
duplak([H|T], H1) -> duplak(T,H).
