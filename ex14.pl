% 's''e''n''d''m''o''r''y'

% 'send' + 'more' = 'money' 
%    =>
% 'd' + 10*'n' + 100*'e' + 1000*'s' + 'e' + 10*'r' + 100*'o' + 1000*'m' = 'y' + 10*'e' + 100*'n' + 1000*'o' + 10000*'m'

escribe_letras([S, E, N, D, M, O, R, Y, _, _]):- 
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,                                                 
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl, !.

encuentra_valores :- Letras = [S,E,N,D,M,O,R,Y,_,_],
                     Digitos = [0,1,2,3,4,5,6,7,8,9],
                     permutation(Letras, Digitos),
                     LEFT is D + 10*N + 101*E + 1000*S + 10*R + 100*O + 1000*M,
                     RIGHT is Y + 10*E + 100*N + 1000*O + 10000*M,
                     LEFT = RIGHT,
                     escribe_letras(Letras). 
