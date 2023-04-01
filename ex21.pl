/* 
Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
Podéis usar la exponenciación, como en 125 is 5**3. El programa (completo) no debe ocupar
más de 3 linea 
*/
log(B,N,L):- 
    between(0, N, L),
    B**L =< N,
    L1 is L+1,
    B**L1 > N,!.