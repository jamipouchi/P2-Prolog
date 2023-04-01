/* 
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
