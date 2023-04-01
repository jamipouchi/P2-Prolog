suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

dados(0,0,[]).
dados(P, N, [V|L]) :- N > 0, member(V, [1,2,3,4,5,6]), N_1 is N - 1, P_V is P - V, dados(P_V, N_1, L).