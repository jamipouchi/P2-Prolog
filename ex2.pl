pescalar([], [], 0).
pescalar([X|XS], [Y|YS], P) :- pescalar(XS, YS, P0), P is P0 + X*Y.