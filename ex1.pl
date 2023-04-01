prod([], 1).
prod([X|XS], P) :- prod(XS, P0), P is X*P0.
