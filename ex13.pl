print_word([]).
print_word([SYMBOL|REST]) :- write(SYMBOL), print_word(REST).

es_palindromo([_]):- !.
es_palindromo([X,X]):- !.
es_palindromo([W|ORD]) :- append(OR, [D], ORD), D == W, es_palindromo(OR), !.

palindromos(L) :- setof(LR, (permutation(L, LR), es_palindromo(LR)), PALINDROMO), print_word(PALINDROMO).