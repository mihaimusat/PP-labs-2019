%functie care construieste perechi de tipul: (element, fiecare element din lista)
pairs(_, [], []) :- !.
pairs(X, [H|T], [(X, H)|R]) :- pairs(X, T, R).

%functie care realizeaza produsul cartezian a doua liste
cartesian(_, [], []) :- !.
cartesian([], _, []) :- !.
cartesian([H|T], L2, R) :- pairs(H, L2, R1), cartesian(T, L2, R2), append(R1, R2, R).

%functie care realizeaza reuniunea a doua liste
union([], _, _) :- !.
union(_, [], _) :- !.
union(L1, L2, R) :- append(L1, L2, L3), setof(E, member(E, L3), R).

%functie care realizeaza intersectia a doua liste 
intersect(_, [], []) :- !.
intersect([], _, []) :- !.
intersect([H|T], L2, [H|R]) :- member(H, L2), !, intersect(T, L2, R).
intersect([_|T], L2, R) :- intersect(T, L2, R).

%functie care realizeaza diferenta dintre doua lise 
diff(_, [], _) :- !.
diff([], _, []) :- !.
diff([H|T], L2, [H|R]) :- not(member(H, L2)), !, diff(T, L2, R).
diff([_|T], L2, R) :- diff(T, L2, R).

%functie care construieste toate submultimile unei liste 
pow([], []) :- !.
pow([H|T], [H|R]) :- pow(T, R).
pow([_|T], R) :- pow(T, R).

%functie care sterge un element din lista si intoarce lista rezultat
remove(H, [H|T], T).
remove(X, [H|T], [H|R]) :- remove(X, T, R).

%functie care genereaza toate permutarile unei liste
perm([], []).
perm([H|T], R) :- perm(T, R1), remove(H, R, R1).

perms(L, R) :- bagof(X, perm(L, X), R).

%functie care insereaza un element intr-o lista la o pozitie oarecare
insert(E, [], [E]).
insert(E, [H|T], [E, H|T]).
insert(E, [H|T], [H|R]) :- insert(E, T, R).

%functie care genereaza aranjamentele de lungime k a unei liste
ar([E|_], 1, [E]).
ar([_|T], K, R) :- ar(T, K, R).
ar([H|T], K, R) :- K \= 1, K1 is K - 1, ar(T, K1, R1), insert(H, R1, R).

ars(L, K, R) :- findall(X, ar(L, K, X), R).

%functie care genereaza combinarile de lungime k a unei liste
comb([E|_], 1, [E]).
comb([_|T], K, R) :- comb(T, K, R).
comb([H|T], K, [H|R]) :- K \= 1, K1 is K - 1, comb(T, K1, R).

combs(L, K, R) :- findall(X, comb(L, K, X), R).



