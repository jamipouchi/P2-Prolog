fib(1, 1).
fib(2, 1).
fib(N, F) :- N > 2, N_1 is N - 1, N_2 is N - 2, fib(N_1, F_1), fib(N_2, F_2), F is F_1 + F_2.