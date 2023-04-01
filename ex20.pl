/*
Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
example:

?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
F=[a,b,c,d,e,f,g,h]?
*/

flatten([], []).
flatten([H|T],F) :- 
    flatten(H,F1), 
    flatten(T,F2), 
    append(F1,F2,F).
flatten(X,[X]).