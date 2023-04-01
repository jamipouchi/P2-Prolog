esta_ordenada([_]).
esta_ordenada([F, S|R]) :- S >= F, esta_ordenada([S|R]).
