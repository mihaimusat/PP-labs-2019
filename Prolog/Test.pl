%functii necesare pentru operatii pe liste
head(L, H) :- L = [H|_].
tail(L, T) :- L = [_|T].
empty(L) :- L = [].
nonempty(L) :- L = [_|_].

%functie care calculeaza lungimea unei liste 
len([],0).
len([_|T], R) :- len(T, R1), R is R1+1.

%functie care inverseaza o lista
rev([], Acc, Acc).
rev([H|T], Acc, R) :- rev(T, [H|Acc], R).
invers(L, R) :- rev(L, [], R). 

%functie care verifica daca un element apartine unei liste
contains(E, [E|_]).
contains(E, [H|T]) :- E \= H, contains(E, T).

%cut demo
f(a).
f(b).
g(a,c).
g(a,d).
g(b,c).
g(b,d).
 
q(X,Y) :- f(X), !, g(X,Y).
q(test,test).

%minimul intre doua numere
min(X, Y, Z) :- X < Y, !, X = Z.
min(X, Y, Y).

%lungimea unei liste folosind un acumulator
accLen([], Acc, Acc).
accLen([_|T], Acc, N) :- Acc1 is Acc + 1, accLen(T, Acc1, N).

len2(L, N) :- accLen(L, 0, N).

%functie care afla maximul dintr-o lista folosind un acumulator
accMax([], Acc, Acc).
accMax([H|T], Acc, Max) :- H > Acc, accMax(T, H, Max), !.
accMax([H|T], Acc, Max) :- H =< Acc, accMax(T, Acc, Max).

maxList(L, Max) :- L = [H|_], accMax(L, H, Max).

%functie care aduna 1 la fiecare element al listei
addone([], []).
addone([H|T], [H1|R]) :- H1 is H + 1, addone(T, R).

%functie care afla minimul dintr-o lista 
accMin([], Acc, Acc).
accMin([H|T], Acc, Min) :- H < Acc, accMin(T, H, Min), !.
accMin([H|T], Acc, Min) :- H >= Acc, accMin(T, Acc, Min).

minList(L, Min) :- L = [H|_], accMin(L, H, Min).

%functie care inmulteste un vector cu un scalar
scalarMult(_, [], []).
scalarMult(X, [H|T], [H1|R]) :- H1 is X*H, scalarMult(X, T, R), !.

%functie care realizeaza produsul scalar intre doi vectori
dotProd([], _, 0).
dotProd(_, [], 0).
dotProd([H1|T1], [H2|T2], R) :- H is H1*H2, dotProd(T1, T2, P), R is H + P, !.

%functie care concateneaza doua liste
concat([], X, X).
concat([H|T], L2, [H|R]) :- concat(T, L2, R).

%functie care afla prefixul unei liste
prefix(P, L) :- concat(P, _, L).

%functie care afla sufixul unei liste
suffix(S, L) :- concat(_, S, L).

%functie care afla sublistele unei liste
sublist(Sl, L) :- suffix(S, L), prefix(Sl, S).

%functie care inverseaza o lista folosind append
rev2([], []).
rev2([H|T], R) :- rev2(T, T1), concat(T1, [H], R).

%functie care verifica ca o lista sa fie "doubled"
doubled([]).
doubled(L) :- append(X, Y, L), X = Y, !.

%functie care verifica daca o lista este palindrom 
palindrome([]).
palindrome(L) :- invers(L, L).

%functie care verifica daca ultimul element e cel dorit
last(L, X) :- invers(L, L1), L1 = [H|_], X is H.

%varianta recursiva
last2([X], X).
last2([_|T], X) :- last2(T, X), !.

%functie care sterge un element dintr-o lista
deleteElem(_, [], []).
deleteElem(E, [E], []).
deleteElem(E, [H|T], [H|R]) :- E \= H, deleteElem(E, T, R), !.
deleteElem(E, [_|T], R) :- deleteElem(E, T, R). 

%functie care verifica daca doua liste sunt egale si ultimul si primul element sunt schimbate intre ele
swapfl([H1|T1], [H2|T2]) :- invers(T1, [H2|X]), invers(T2, [H1|X]).

class(Number,positive)  :-  Number  >  0, !. 
class(0,zero) :- !. 
class(Number,negative)  :-  Number  <  0.

loves(vincent,mia). 
loves(marsellus,mia). 
loves(pumpkin,honey_bunny). 
loves(honey_bunny,pumpkin). 
    
jealous(X, X) :- fail.
jealous(X,Y):-  loves(X,Z),  loves(Y,Z).

%functie care imparte o lista in doua liste : una de nr pozitive si una de nr negative
split([], [], []).
split([H|T], [H|P], N) :- H >= 0, split(T, P, N), !.
split([H|T], P, [H|N]) :- H < 0, split(T, P, N).

%functie care intoarce true daca doi termeni nu unifica
nu(X, Y) :- \+ (X = Y).

%functie care verifica daca un element este un membru al unei liste
member(E, [E|_]).
member(E, [_|T]) :- member(E, T), !.

member2(E, L) :- append(_, [E|_], L).

%functie care intoarce lista fara duplicate
set([], Acc, Acc).
set([H|T], Acc, R) :- member(H, Acc), set(T, Acc, R), !.
set([H|T], Acc, R) :- not(member(H, Acc)), set(T, [H|Acc], R).

%functie care face flatten pe o lista
flatten([], Acc, Acc).
flatten([H|T], Acc, R) :- flatten(T, Acc, X), flatten(H, X, R).
flatten(X, Acc, [X|Acc]) :- not(is_list(X)).

%functie care face suma elementelor de la 1 la n
:- dynamic sigmares/2.

sigmares(1, 1).

sigma(X, Y) :-
    sigmares(X, Y), !.

sigma(X, Y) :-
    Xp is X - 1,
    sigma(Xp, R),
    Y is R + X,
    assert(sigmares(X, Y)).

%functie care genereaza toate submultimile unei liste
subset([], []) :- !.
subset([H|T], [H|R]) :- subset(T, R).
subset(T, [_|R]) :- subset(T, R).

%functie care calculeaza powerset-ul unui set
powerset(L, R) :- findall(X, subset(X, L), R).

%exemplu recursivitate
directTrain(saarbruecken,dudweiler). 
directTrain(forbach,saarbruecken). 
directTrain(freyming,forbach). 
directTrain(stAvold,freyming). 
directTrain(fahlquemont,stAvold). 
directTrain(metz,fahlquemont).
directTrain(nancy,metz).

travelFromTo(X, Y) :- directTrain(X, Y), !.
travelFromTo(X, Y) :- directTrain(X, Z), travelFromTo(Z, Y), !.

%adunare pentru numere naturale
add(0, Y, Y).
add(succ(X), Y, succ(Z)) :- add(X, Y, Z).

%scaderea pentru numere naturale
sub(Y, 0, Y) :- !.
sub(X, X, 0) :- !.
sub(succ(X), Y, succ(Z)) :- sub(X, Y, Z), !.

%functie care converteste un succ in numar
conv2(0, Acc, Acc).
conv2(succ(X), Acc, R) :- Acc1 is Acc + 1, conv2(X, Acc1, R).

conv(X, Y) :- conv2(X, 0, Y). 

%functie care compara doua numere naturale
greater_than(X, Y) :- sub(X, Y, Z), conv(Z, R), R > 0.

%functie care face mirror unui arbore binar
swap(leaf(X), leaf(X)).
swap(tree(X, Y), tree(Z, T)) :- swap(X, T), swap(Y, Z).

%functie care verifica daca un element e al doilea element din lista
second(X, L) :- L = [_, H2 | _], X = H2.

%functie care verifica daca primele doua elemente ale listelor sunt interschimbate 
swap12([H1, H2|T1], [H3, H4|T2]) :- H1 = H4, H2 = H3, T1 = T2.

%translateaza fiecare cuvant din lista in echivalentul in limba engleza
tran(eins,one). 
tran(zwei,two). 
tran(drei,three). 
tran(vier,four). 
tran(fuenf,five). 
tran(sechs,six). 
tran(sieben,seven). 
tran(acht,eight). 
tran(neun,nine).

listtran([], Acc, Acc).
listtran([H|T], Acc, [H1|R]) :- tran(H, H1), listtran(T, Acc, R), !.   

listtran(L, R) :- listtran(L, [], R).

%functie care duplica fiecare element dintr-o lista
twice([], Acc, Acc).
twice([H|T], Acc, [H, H|R]) :- twice(T, Acc, R), !. 

twice(L, R) :- twice(L, [], R).

%functie care combina doua liste element cu element
combine1(X, [], X).
combine1([], X, X).
combine1([H1|T1], [H2|T2], [H1, H2|R]) :- combine1(T1, T2, R), !. 

%functie care combina "altfel" doua liste
combine2([], [], []).
combine2([H1|T1], [H2|T2], [[H1, H2]|R]) :- combine2(T1, T2, R), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%functie care intoarce a n-a linie dintr-o matrice
line_n([H|_], 1, H).
line_n([H|T], N, R) :- N1 is N - 1, line_n(T, N1, R), !.

%functie care intoarce primul element dintr-o lista
first([X], X) :- !.
first([H|T], H).
 
%functie care verifica daca primul si ultimul element sunt egale
sameEnds(L) :- first(L, X), last(L, Y), X = Y.

%functie care verifica daca o lista este sortata
is_sorted([]).
is_sorted([X]).
is_sorted([H1, H2|T]) :- H1 =< H2, is_sorted([H2|T]), !.

%functie care filtreaza elementele mai mari decat pragul P
filter(L, P, R) :- findall(X, (member(X, L), X > P), R).

%functie care identifica secventele crescatoare de cel putin doua elemente
up([], []).
up([H1, H2|T], [H1, H2|R]) :- H1 < H2, !, up(T, H1, R).
up([_|T], R) :- up(T, R).

up([], _, []) :- !.
up([H|T], E, [H|R]) :- H > E, !, up(T, H, R).

up(L, _, R) :- up(L, R).

%functie care interclaseaza doua liste sortate crescator
merge([], X, X).
merge(X, [], X).
merge([H1|T1], [H2|T2], [H1|L]) :- H1 =< H2, merge(T1, [H2|T2], L), !.
merge([H1|T1], [H2|T2], [H2|L]) :- merge([H1|T1], T2, L).

%functie care realizeaza inserarea sortata intr-o lista
insertOrd(E, [], [E]) :- !.
insertOrd(E, [H|T], [E,H|T]) :- E =< H, !.
insertOrd(E, [H|T], [H|R]) :- insertOrd(E, T, R).

%functie care realizeaza insertion sort
isort([], []).
isort([H|T], R) :- isort(T, T1), insertOrd(H, T1, R).

%functie care grupeaza aparitiile consecutive ale unei liste
group([], []).
group([H1|T], [H2|R]) :- transfer(H1, T, Rest, H2), group(Rest, R), !.

%functie in care Rest = lista care ramane din T dupa ce toate aparitiile lui H1, sunt scoase si puse in H2
transfer(X, [], [], [X]).
transfer(X, [Y|T], [Y|T], [X]) :- X \= Y.
transfer(X, [X|T], Rest, [X|R]) :- transfer(X, T, Rest, R).

%functie care elimina numerele impare dintr-o lista
getEven([], Acc, Acc).
getEven([H|T], Acc, R) :- 0 is mod(H, 2), getEven(T, [H|Acc], R), !.
getEven([H|T], Acc, R) :- getEven(T, Acc, R). 

%functie care afla ultimul element al unei liste
last3(L, X) :- invers(L, R), R = [H|_], X = H.

%functie care verifica daca doua liste sunt egale
eqLists([], []) :- !.
eqLists([H], [H]) :- !.
eqLists([H1|T1], [H2|T2]) :- H1 = H2, eqLists(T1, T2).

%functie care numara de cate ori apare un element intr-o lista
nrApp(X, [], 0).
nrApp(X, [H|T], N) :- X = H, nrApp(X, T, N1), N is N1 + 1, !.
nrApp(X, [H|T], N) :- nrApp(X, T, N).

%functie care determina numarul de elemente mai mari ca A si mai mici ca B dintr-o lista
range([], _, _, 0).
range([H|T], A, B, N) :- H >= A, H =< B, range(T, A, B, N1), N is N1 + 1, !.
range([H|T], A, B, N) :- range(T, A, B, N).

%functie care calculeaza maximul dintr-o lista
maxx([], 0).
maxx([H|T], M) :- maxx(T, M1), H > M1, M = H, !.
maxx([H|T], M) :- maxx(T, M).

%functie care functioneaza ca zip din Haskell
makePairs([], X, []) :- !.
makePairs(X, [], X) :- !.
makePairs([H1|T1], [H2|T2], [(H1, H2)|R]) :- makePairs(T1, T2, R).

%functie care functioneaza ca unzip din Haskell
splitPairs([], [], []) :- !.
splitPairs([(H1,H2)|R], [H1|T1], [H2|T2]) :- splitPairs(R, T1, T2).

%functie care face diferenta dintre multimi 
diff(X, [], X) :- !.
diff([], X, []) :- !.
diff([H1|T1], L, [H1|R]) :- not(member(H1, L)), diff(T1, L, R), !.
diff([H1|T1], L, R) :- diff(T1, L, R).

%functie care face intersectia a doua multimi
intersect(X, [], []) :- !.
intersect([], X, []) :- !.
intersect([H|T], L, [H|R]) :- member(H, L), intersect(T, L, R), !.
intersect([H|T], L, R) :- intersect(T, L, R).

%implementare one_bubble 
one_bubble([], []).
one_bubble([X], [X]).
one_bubble([X,Y|T], [X|R]) :- X =< Y, !, one_bubble([Y|T], R).
one_bubble([X,Y|T], [Y|R]) :- one_bubble([X|T], R).

%implementare bubble sort
bubble_sort(L, L) :- one_bubble(L, L), !.
bubble_sort(L, S) :- one_bubble(L, Bubbled), bubble_sort(Bubbled, S).

%functie care face switch elementelor pana gaseste un 0
schimba(a, b).
schimba(b, a).

switchZero([], []).
switchZero([0|T], [0|T]) :- !.
switchZero([H|T], [P|R]) :- schimba(H, P), !, switchZero(T, R).
switchZero([H|T], [H|R]) :- switchZero(T, R).
 
%functie care verifica daca lista contine un singur numar par
only_one(X) :- getEven(X, [], X1), length(X1, R), R = 1.

