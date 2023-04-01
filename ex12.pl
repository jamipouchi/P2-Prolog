word_builder_alphabetically(_, 0, []):- !.
word_builder_alphabetically(A, N, [SYMBOL|REST]) :- N > 0, append(_, [SYMBOL|NEXT], A), N_1 is N - 1, word_builder_alphabetically([SYMBOL|NEXT], N_1, REST).

word_builder(_, 0, []) :- !.
word_builder(A, N, [SYMBOL|REST]) :- N > 0, N_1 is N - 1, member(SYMBOL, A), word_builder(A, N_1, REST).

print_word([]).
print_word([SYMBOL|REST]) :- write(SYMBOL), print_word(REST). 

diccionario(A, N) :- word_builder(A, N, WORD), print_word(WORD).