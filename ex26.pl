% Complete the following predicate in prolog.
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