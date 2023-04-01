esta_ordenada([_]).
esta_ordenada([F, S|R]) :- S >= F, esta_ordenada([S|R]).

ord(L1, L2) :- permutation(L1, L2), esta_ordenada(L2).