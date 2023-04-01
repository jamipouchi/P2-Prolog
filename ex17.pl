/*
Complete the following backtracking procedure for SAT in Prolog. Program everything, except
the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
For example, p3 ∨ ¬p6 ∨ p2 is represented by [3,-6,2]. Do things as simple as possible.
*/

p:- readclauses(F), sat([],F).
p:- write("UNSAT"),nl.
sat(I,[]):- write("IT IS SATISFIABLE. Model: "), write(I),nl,!.
sat(I,F):- 
            decision_lit(F,Lit), % Selects a literal
            write(Lit),nl, % Prints the literal (for debugging purposes)
            simplif(Lit,F,F1), % Simplifies the formula
            write(F1),nl, % Prints the simplified formula (for debugging purposes)
            sat([Lit|I],F1). % Tries to satisfy the formula with Lit
sat(I,F):-
            decision_lit(F,Lit), % Selects a literal
            simplif(-Lit,F,F2), % Simplifies the formula with the negation of Lit
            sat([-Lit|I],F2). % Tries to satisfy the formula with ¬Lit

% Simplifies F. Warning: may fail and cause backtracking
simplif(Lit,F,F1):- exclude(member(Lit), F, F1). % Removes clauses containing Lit

readclauses(F). % Reads the clauses from the input. Not asked to be implemented

% This isn't great, as it can select the negation of a literal already selected. 
% It should check that no negation of Lit is selected, but that would require haaving I as a parameter.
decision_lit(F,Lit) :- member([Lit], F), !. % Select unit clause if any
decision_lit([[Lit|_]|_],Lit). % Select an arbitrary one
