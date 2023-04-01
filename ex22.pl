/* 
Supongamos que N estudiantes (identificados por un numero entre 1 y N) se quieren matricular
de LI, pero solo hay espacio para M, con M < N. Ademas nos dan una lista L de pares de estos
estudiantes que son incompatibles entre sı (por ejemplo, porque siempre se copian). Queremos
obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
con M de los N estudiantes tal que si [x, y] ∈ L entonces {x, y} ̸⊆ S. Por ejemplo, una solucion de
li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1] .
Escribe una version lo mas sencilla que puedas, aunque sea ineficiente, del estilo “generar una
solucion (total) y despues comprobar si es correcta”. 
*/

/* 
li(N,M,L,S):- 
    list(N, Llista),
    permutation(Llista, Desordre),                          % This is too expensive.
    append(_, S, Desordre),
    length(S, M),
    check(L, S).
check(L, S) :-
    member(X, S),
    member([X,Y], L),
    member(Y, S), !, fail,
    member([Y,X], L),
    member(Y, S), !, fail.
% list of numbers from 1 to N
list(N, L) :- list(N, 1, L).
list(N, N, [N]) :- !.
list(N, I, [I|L]) :- I < N, I1 is I+1, list(N, I1, L). 
*/

% AAaaand this doesn't work either. Too slow again? I don't know.

li(_, 0, _, []):- !.
li(N,M,L,[X|S1]) :-
    between(1,N,X),
    M1 is M - 1,
    li(N,M1,L,S1),
    check(L,[X|S1]).

check(_,[_]).
check(L,[X|[Y|Ss]]) :-
    Y \= X,
    \+ member([X,Y],L),
    \+ member([Y,X],L),
    check(L, [X|Ss]).

% This problem can be written as a CNF formula, and then solved with a sat - solver.
% There is N literals, from 1 to N, and M of them must be true.
% There are length(L) clauses, each one of them is a pair of literals, and they are incompatible.
% The clauses are as such: [x, y] ∈ L => ¬x ∨ ¬y