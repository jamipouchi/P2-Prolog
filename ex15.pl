% der( Expr, Var, Der )  == "la derivada de Expr con respecto Var es Der"
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
simplifica(X-X, 0).