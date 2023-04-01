suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

suma_ants(L) :- append(L1, [X|_], L), suma_lista(L1, V_L1), X is V_L1.