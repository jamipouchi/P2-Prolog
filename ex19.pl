/*
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
