suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

suma_demas(L) :- append(L1, [X|L2], L), suma_lista(L1, S_L1), suma_lista(L2, S_L2), X is S_L1 + S_L2.