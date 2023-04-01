ultimo_elemento(L, X) :- append(_, [X], L).

lista_inversa([], []).
lista_inversa(LISTA, [A| STIL]) :- append(LIST, [A], LISTA), lista_inversa(LIST, STIL).