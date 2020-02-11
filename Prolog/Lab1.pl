%functie care intoarce primele doua elemente din lista 
firstTwo([X, Y|_], X, Y).

%functie care intoarce true daca elementul cautat nu e in lista
notContains(E, []).
notContains(E, [H|T]) :- E \= H, notContains(E, T), !.

%functie care sterge un element si intoarce lista
deleteElem(E, [], []).
deleteElem(E, [H|T], [H|R]) :- E \= H, deleteElem(E, T, R), !.
deleteElem(E, [H|T], R) :- deleteElem(E, T, R). 

%functie care sterge duplicatele dintr-o lista
unique([], []).
unique([H|T], [H|R]) :- deleteElem(H, T, R1), unique(R1, R), !.

%functie care verifica daca inputul este o lista
isList([]).
isList([_|T]) :- isList(T).

%functie care intoarce doar elementele de tip lista 
listOnly([], []).
listOnly([H|T], [H|R]) :- isList(H), listOnly(T, R), !.
listOnly([H|T], R) :- listOnly(T, R).

%functie care realizeza inserarea ordonata intr-o lista
insertOrd(E, [], [E]) :- !.
insertOrd(E, [H|T], [E,H|T]) :- E =< H, !.
insertOrd(E, [H|T], [H|R]) :- insertOrd(E, T, R).

%functie care realizeaza insert sort
insertSort([], []).
insertSort([H|T], R) :- insertSort(T, R1), insertOrd(H, R1, R). 

%arbore de test
tree(1, tree(2, tree(4, null, null), tree(5,null,null)), tree(3, null, null)).

%functie care calculeaza numarul de noduri pentru un arbore
size(null, 0).
size(tree(_, L, R), X) :- size(L, L1), size(R, R1), X is 1 + L1 + R1.

%functie care realizeza maximul intre 2 numere.
maxim(X, Y, Z) :- X > Y, Z = X, !.
maxim(X, Y, Z) :- X =< Y, Z = Y.

%functie care calculeaza numarul de niveluri ale unui arbore
height(null, 0).
height(tree(_, L, R), X) :- height(L, H1), height(R, H2), maxim(H1, H2, Hm), X is 1+Hm.

%functie care concateneaza doua liste
concat([], L, L).
concat([H|T], L1, [H|R]) :- concat(T, L1, R).

%functie care pune nodurile in lista in preordine
flatten(null, []).
flatten(tree(K, L, R), P) :- flatten(L, L1), flatten(R, R1), concat([K], L1, P1), concat(P1, R1, P).



