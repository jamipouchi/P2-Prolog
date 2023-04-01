pert(X, [X|_]).
pert(X, [_|TAIL]) :- pert(X, TAIL).

unio([], Y, Y).
unio([X|XS], YS, [X|L3]) :- \+ pert(X, YS), unio(XS, YS, L3).
unio([_|XS], YS, L3) :- unio(XS, YS, L3).

interseccio([], _, []).
interseccio([X|XS], YS, [X|L3]) :- pert(X, YS), interseccio(XS, YS, L3).
interseccio([_|XS], YS, L3) :- interseccio(XS, YS, L3).