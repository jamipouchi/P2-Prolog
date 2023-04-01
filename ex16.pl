% Given a list of domino tiles, print a chain using all tiles, or print "no hay cadena" if it isn't possible
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
p(L,[f(A,B)|P]) :- select(f(B,A),L,R), p(R,P).