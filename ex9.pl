count_remove(Y, L, L, 0) :- \+ member(Y, L).
count_remove(X, [X|XS], LF, N) :- count_remove(X, XS, LF, N_1), N is N_1 + 1.
count_remove(Y, [X|XS], [X|LF], N) :- count_remove(Y, XS, LF, N).

cards([], []).
cards([X|XS], [[X, N]|R0]) :- count_remove(X, [X|XS], YS, N), cards(YS, R0).

card(L) :- cards(L, RES), write(RES).