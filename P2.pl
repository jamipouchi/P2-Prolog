%%%%%%%%%%ex1%%%%%%%%%%prod([], 1).
prod([X|XS], P) :- prod(XS, P0), P is X*P0.
%%%%%%%%%%ex2%%%%%%%%%%pescalar([], [], 0).
pescalar([X|XS], [Y|YS], P) :- pescalar(XS, YS, P0), P is P0 + X*Y.%%%%%%%%%%ex3%%%%%%%%%%pert(X, [X|_]).
pert(X, [_|TAIL]) :- pert(X, TAIL).

unio([], Y, Y).
unio([X|XS], YS, [X|L3]) :- \+ pert(X, YS), unio(XS, YS, L3).
unio([_|XS], YS, L3) :- unio(XS, YS, L3).

interseccio([], _, []).
interseccio([X|XS], YS, [X|L3]) :- pert(X, YS), interseccio(XS, YS, L3).
interseccio([_|XS], YS, L3) :- interseccio(XS, YS, L3).%%%%%%%%%%ex4%%%%%%%%%%ultimo_elemento(L, X) :- append(_, [X], L).

lista_inversa([], []).
lista_inversa(LISTA, [A| STIL]) :- append(LIST, [A], LISTA), lista_inversa(LIST, STIL).%%%%%%%%%%ex5%%%%%%%%%%fib(1, 1).
fib(2, 1).
fib(N, F) :- N > 2, N_1 is N - 1, N_2 is N - 2, fib(N_1, F_1), fib(N_2, F_2), F is F_1 + F_2.%%%%%%%%%%ex6%%%%%%%%%%suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

dados(0,0,[]).
dados(P, N, [V|L]) :- N > 0, member(V, [1,2,3,4,5,6]), N_1 is N - 1, P_V is P - V, dados(P_V, N_1, L).%%%%%%%%%%ex7%%%%%%%%%%suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

suma_demas(L) :- append(L1, [X|L2], L), suma_lista(L1, S_L1), suma_lista(L2, S_L2), X is S_L1 + S_L2.%%%%%%%%%%ex8%%%%%%%%%%suma_lista([], 0).
suma_lista([X|XS], V) :- suma_lista(XS, V0), V is V0 + X.

suma_ants(L) :- append(L1, [X|_], L), suma_lista(L1, V_L1), X is V_L1.%%%%%%%%%%ex9%%%%%%%%%%count_remove(Y, L, L, 0) :- \+ member(Y, L).
count_remove(X, [X|XS], LF, N) :- count_remove(X, XS, LF, N_1), N is N_1 + 1.
count_remove(Y, [X|XS], [X|LF], N) :- count_remove(Y, XS, LF, N).

cards([], []).
cards([X|XS], [[X, N]|R0]) :- count_remove(X, [X|XS], YS, N), cards(YS, R0).

card(L) :- cards(L, RES), write(RES).%%%%%%%%%%ex10%%%%%%%%%%esta_ordenada([_]).
esta_ordenada([F, S|R]) :- S >= F, esta_ordenada([S|R]).
%%%%%%%%%%ex11%%%%%%%%%%esta_ordenada([_]).
esta_ordenada([F, S|R]) :- S >= F, esta_ordenada([S|R]).

ord(L1, L2) :- permutation(L1, L2), esta_ordenada(L2).%%%%%%%%%%ex12%%%%%%%%%%word_builder_alphabetically(_, 0, []):- !.
word_builder_alphabetically(A, N, [SYMBOL|REST]) :- N > 0, append(_, [SYMBOL|NEXT], A), N_1 is N - 1, word_builder_alphabetically([SYMBOL|NEXT], N_1, REST).

word_builder(_, 0, []) :- !.
word_builder(A, N, [SYMBOL|REST]) :- N > 0, N_1 is N - 1, member(SYMBOL, A), word_builder(A, N_1, REST).

print_word([]).
print_word([SYMBOL|REST]) :- write(SYMBOL), print_word(REST). 

diccionario(A, N) :- word_builder(A, N, WORD), print_word(WORD).%%%%%%%%%%ex13%%%%%%%%%%print_word([]).
print_word([SYMBOL|REST]) :- write(SYMBOL), print_word(REST).

es_palindromo([_]):- !.
es_palindromo([X,X]):- !.
es_palindromo([W|ORD]) :- append(OR, [D], ORD), D == W, es_palindromo(OR), !.

palindromos(L) :- setof(LR, (permutation(L, LR), es_palindromo(LR)), PALINDROMO), print_word(PALINDROMO).%%%%%%%%%%ex14%%%%%%%%%%% 's''e''n''d''m''o''r''y'

% 'send' + 'more' = 'money' 
%    =>
% 'd' + 10*'n' + 100*'e' + 1000*'s' + 'e' + 10*'r' + 100*'o' + 1000*'m' = 'y' + 10*'e' + 100*'n' + 1000*'o' + 10000*'m'

escribe_letras([S, E, N, D, M, O, R, Y, _, _]):- 
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,                                                 
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl, !.

encuentra_valores :- Letras = [S,E,N,D,M,O,R,Y,_,_],
                     Digitos = [0,1,2,3,4,5,6,7,8,9],
                     permutation(Letras, Digitos),
                     LEFT is D + 10*N + 101*E + 1000*S + 10*R + 100*O + 1000*M,
                     RIGHT is Y + 10*E + 100*N + 1000*O + 10000*M,
                     LEFT = RIGHT,
                     escribe_letras(Letras). 
%%%%%%%%%%ex15%%%%%%%%%%% der( Expr, Var, Der )  == "la derivada de Expr con respecto Var es Der"
der( X, X, 1) :- !.
der( C, _, 0):- atomic(C).     % atomic significa que es una expresion constante o un entero
der( A+B, X, U+V ):- der(A,X,U), der(B,X,V). 
der( A*B, X, A*V+B*U ):- der(A,X,U), der(B,X,V). 

% simplifica( Expr, Simplificacio)

simplifica(C, C) :- atomic(C), write("0 -> "), write(C).
simplifica(X+0, X).
simplifica(0+X, X).
simplifica(X*0, 0).
simplifica(0*X, 0).
simplifica(X-X, 0).%%%%%%%%%%ex16%%%%%%%%%%% Given a list of domino tiles, print a chain using all tiles, or print "no hay cadena" if it isn't possible
% Example:
%?- dom( [ f(3,4), f(2,3), f(1,6), f(2,2), f(4,2), f(2,1) ] ).
% prints the correct chain:
% [ f(2,3), f(3,4), f(4,2), f(2,2), f(2,1), f(1,6) ]

% a) p is the predicate that generates all possible chains
p([],[]).
p(L,[X|P]) :- select(X,L,R), p(R,P).


dom(L) :- p(L,P), ok(P), write(P), nl.
dom( ) :- write("no hay cadena"), nl.

% b) Checks if it is a correct chain
ok([f(_,_)]).
ok([f(_,B),f(C,D)|T]) :- B = C, ok([f(C,D)|T]).

% c) p extended so it can flip tiles
p(L,[f(A,B)|P]) :- select(f(B,A),L,R), p(R,P).%%%%%%%%%%ex17%%%%%%%%%%/*
Complete the following backtracking procedure for SAT in Prolog. Program everything, except
the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
For example, p3 ∨ ¬p6 ∨ p2 is represented by [3,-6,2]. Do things as simple as possible.
*/

p:- readclauses(F), sat([],F).
p:- write("UNSAT"),nl.
sat(I,[]):- write("IT IS SATISFIABLE. Model: "), write(I),nl,!.
sat(I,F):- 
            decision_lit(F,Lit), % Selects a literal
            write(Lit),nl, % Prints the literal (for debugging purposes)
            simplif(Lit,F,F1), % Simplifies the formula
            write(F1),nl, % Prints the simplified formula (for debugging purposes)
            sat([Lit|I],F1). % Tries to satisfy the formula with Lit
sat(I,F):-
            decision_lit(F,Lit), % Selects a literal
            simplif(-Lit,F,F2), % Simplifies the formula with the negation of Lit
            sat([-Lit|I],F2). % Tries to satisfy the formula with ¬Lit

% Simplifies F. Warning: may fail and cause backtracking
simplif(Lit,F,F1):- exclude(member(Lit), F, F1). % Removes clauses containing Lit

readclauses(F). % Reads the clauses from the input. Not asked to be implemented

% This isn't great, as it can select the negation of a literal already selected. 
% It should check that no negation of Lit is selected, but that would require haaving I as a parameter.
decision_lit(F,Lit) :- member([Lit], F), !. % Select unit clause if any
decision_lit([[Lit|_]|_],Lit). % Select an arbitrary one
%%%%%%%%%%ex18%%%%%%%%%%/*
Consider two groups of 10 people each. In the first group, as expected, the percentage of people
with lung cancer among smokers is higher than among non-smokers. In the second group, the
same is the case. But if we consider the 20 people of the two groups together, then the situation
is the opposite: the proportion of people with lung cancer is higher among non-smokers than
among smokers! Can this be true? Write a little Prolog program to find it out.
*/

% It gives false! So, it cannot be true. Let's do some math to understand why.

/*
s1 = smokers in group 1
c1 = lung cancer in group 1
s2 = smokers in group 2
c2 = lung cancer in group 2

s1/c1 > (10 - s1)/c1 && s2/c2 > (10 - s2)/c2

- Multiply first equation by (c1 + c2) / c1 && Multiply second equation by (c1 + c2) / c2
=>
s1/(c1 + c2) > (10 - s1)/(c1 + c2) && s2/(c1 + c2) > (10 - s2)/(c1 + c2)

- Add the two equations
=>
(s1 + s2) / (c1 + c2) > (20 - s1 - s2) / (c1 + c2)

Thus, if we have more proportion of lung cancer given smoker in each group, the global proportion of lung cancer given smoker is higher than non-smoker.
*/

group(1, 10).
group(2, 10).

smokers(GROUP, NUM_SMOKERS, NUM_NON_SMOKERS) :- 
    group(GROUP, People), 
    between(1, People, NUM_SMOKERS),                                    % at least 1 smoker, to fulfill the condition of the problem
    NUM_NON_SMOKERS is People - NUM_SMOKERS.

lung_cancer(GROUP, NUM_LUNG_CANCER) :- 
    group(GROUP, People), 
    between(1, People, NUM_LUNG_CANCER).                                % at least 1 lung cancer, to fulfill the condition of the problem

check_perc(NUM_SMOKERS, NUM_NON_SMOKERS, NUM_LUNG_CANCER) :- 
    PERC_LUNG_CANCER_GIVEN_SMOKER is NUM_SMOKERS / NUM_LUNG_CANCER, 
    PERC_LUNG_CANCER_GIVEN_NON_SMOKER is NUM_NON_SMOKERS / NUM_LUNG_CANCER,
    PERC_LUNG_CANCER_GIVEN_SMOKER > PERC_LUNG_CANCER_GIVEN_NON_SMOKER.

two_group_perc_lung_cancer(GROUP1, GROUP2) :-
    smokers(GROUP1, NUM_SMOKERS1, NUM_NON_SMOKERS1),                    % smokers for group 1
    lung_cancer(GROUP1, NUM_LUNG_CANCER1),                              % lung cancer for group 1
    check_perc(NUM_SMOKERS1, NUM_NON_SMOKERS1, NUM_LUNG_CANCER1),       % check if  perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in group 1
    smokers(GROUP2, NUM_SMOKERS2, NUM_NON_SMOKERS2),                    % smokers for group 2
    lung_cancer(GROUP2, NUM_LUNG_CANCER2),                              % lung cancer for group 2
    check_perc(NUM_SMOKERS2, NUM_NON_SMOKERS2, NUM_LUNG_CANCER2),       % check if  perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in group 2
    TOTAL_SMOKERS is NUM_SMOKERS1 + NUM_SMOKERS2,                       % total smokers
    TOTAL_NON_SMOKERS is NUM_NON_SMOKERS1 + NUM_NON_SMOKERS2,           % total non-smokers
    TOTAL_NUM_CANCER is NUM_LUNG_CANCER1 + NUM_LUNG_CANCER2,            % total lung cancer
    \+ check_perc(TOTAL_SMOKERS, TOTAL_NON_SMOKERS, TOTAL_NUM_CANCER).  % check if  NOT perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in total
%%%%%%%%%%ex19%%%%%%%%%%/*
Supongamos que tenemos una máquina que dispone de monedas de valores [X1,...Xn] y tiene
que devolver una cantidad C de cambio utilizando el mínimo número de monedas. Escribe un
programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).
*/
maq(L,C,M) :- sort(0, >, L,L1), maq1(L1,C,M), write(M).


% This is a greedy approach, which might not work in all cases.
maq1([],0,[]).              % Base case
maq1([X|Xs],C,[M|Ms]) :-    % Recursive case, when we can select a coin
    C >= X, C1 is C-X, 
    maq1([X|Xs],C1,[M1|Ms]), 
    M is M1+1.
maq1([_|Xs],C,[0|Ms]) :-    % Recursive case, when we can't select a coin, and we go to the next
    maq1(Xs,C,Ms).
%%%%%%%%%%ex20%%%%%%%%%%/*
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
flatten(X,[X]).%%%%%%%%%%ex21%%%%%%%%%%/* 
Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
Podéis usar la exponenciación, como en 125 is 5**3. El programa (completo) no debe ocupar
más de 3 linea 
*/
log(B,N,L):- 
    between(0, N, L),
    B**L =< N,
    L1 is L+1,
    B**L1 > N,!.%%%%%%%%%%ex22%%%%%%%%%%/* 
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
% The clauses are as such: [x, y] ∈ L => ¬x ∨ ¬y%%%%%%%%%%ex23%%%%%%%%%%/* Given a list of integers L, and a maximum sum K, write the subsets Sm of L such that:
- sum(Sm) =< K, and
- no element in L \ Sm can be added to Sm without exceeding the sum K.
For the example below, a correct output would be the following (or in another order):
[2,5,-2,1]
[2,-2,2,3,1]
[2,-2,2,4]
[2,-2,4,1]
[5,-2,2,1]
[5,-2,3]
[7,-2,1]
[-2,2,4,1]
[-2,3,4,1]
Hint: you can use the predicate sum list(L, X), which is true if X is the sum of the numbers
in L; e.g., sum list([1,2,3], 6) holds.
Complete: 
*/

%% Example:
numbers([2,5,7,-2,2,9,3,4,1]).
maxSum(6).

%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([X|L], [X|Subset], Rest):- 
    
    subsetWithRest(L, Subset, Rest).
subsetWithRest([X|L], Subset, [X|Rest]):-
    subsetWithRest(L, Subset, Rest).

%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that
%% it sums at most K
%% and if we try to add any other element, the sum exceeds K.
maxSubset(K, L, Sm):-
    subsetWithRest(L, Sm, Rest),
    sum_list(Sm, Sum),
    Sum =< K,
    \+ (
        member(X, Rest), 
        Sum1 is Sum + X, 
        Sum1 =< K
        ).

main :-
    numbers(L), maxSum(K),
    maxSubset(K, L, Sm),
    write(Sm), nl, fail.
main:- halt.%%%%%%%%%%ex24%%%%%%%%%%/* 
Given a graph declared as in the example below, write all its cliques of size at least minCliqueSize.
Remember, a clique is a complete subgraph: a subset {textttS of the vertices such that for all
U,V in S there is an edge U-V.
For the example below, a correct output would be the following (or in another order):
[2,4,5,7,9]
[2,4,5,7]
[2,4,5,9]
[2,4,7,9]
[2,4,8,9]
[2,5,7,9]
[4,5,7,9]
Complete: 
*/

%%==== Example: ========================================================
numVertices(10).
minCliqueSize(4).
vertices(Vs):- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V):- vertices(Vs), member(V,Vs).
edge(U,V):- edge1(U,V).
edge(U,V):- edge1(V,U).
edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).
%%==========================================================

main:- 
    vertices(Vs), 
    subconjunto(Vs,S),
    length(S,Llargada),
    minCliqueSize(Mida_minima),
    Llargada >= Mida_minima,
    isClique(S),
    write(S), nl, fail.

main:- halt.

isClique(S):-
    forall(member(U,S), forall((member(V,S), V \= U), edge(U,V))).

subconjunto([],[]).
subconjunto([X|C],[X|S]) :-
    subconjunto(C,S).
subconjunto([_|C],L2) :-
    subconjunto(C,L2).
%%%%%%%%%%ex25%%%%%%%%%%/* 
Complete the following predicate in prolog.
% nthRoot( N, K, R ) === "Given positive integers N and K,
the integer part of the Nth root of K is R".
% Example: the integer part of the 2th root (square root) of 16 is 4.
% Example: the integer part of the 3rd root (cubic root) of 8 is 2.
% Example: the integer part of the 4th root of 16 is 2.
% Example: the integer part of the 4th root of 15 is 1.
*/
nthRoot( N, K, R ):- between( 1, K, R ), R**N =< K, R1 is R + 1, R1**N > K.

% This problem is the same as log(B,N,L), solved in ex. 21
% nthRoot( N, K, R ):- log( N, K, R ).
%%%%%%%%%%ex26%%%%%%%%%%% Complete the following predicate in prolog.
% allSSSS(L) (allSquareSummingSubSequences) ===
% "Given a sequence of positive integers L, write all non-empty subsequences of L
% whose sum is a perfect square, in the following format":
% ?- allSSSS([6,3,4,5,6,9,8,5,2,3,4]).
% 9-[6,3]
% 49-[3,4,5,6,9,8,5,2,3,4]
% 4-[4]
% 9-[4,5]
% 9-[9]
% 9-[2,3,4]
% 4-[4]

allSSSS(L) :- subgrupo(L, S), sumSquare(S), sum_list(S, X), write(X), write("-"), write(S), nl, fail.

subgrupo([], []).
subgrupo([X|L], [X|S]) :- subgrupo(L, S).
subgrupo([_|L], S) :- subgrupo(L, S).

sumSquare(S) :- sum_list(S, X), floor(sqrt(X))^2 =:= X.