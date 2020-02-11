--functie care intoarce ultimul element al unei liste
myLast [] = error "The list is empty!"
myLast [x] = x
myLast (x:xs) = myLast xs

--functie care intoarce penultimul element al unei liste
notLast [] = error "The list is empty!"
notLast [x] = error "There are not enough elements!"
notLast (x:xs) = if (length xs == 1) then x 
		 else notLast xs

--functie care intoarce al k-lea element din lista
elementAt k l = l !! (k - 1)

--numarul de elemente dintr-o lista
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--inversarea unei liste
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--functie care verifica daca o lista este palindrom
isPalindrome l = checkAux l (reverse l) where 
			checkAux [] [] = True
			checkAux (x:xs) (y:ys) = (x == y) && (xs == ys)

--functie care elimina duplicatele consecutive dintr-o lista
elimDups [] = []
elimDups [x] = [x]
elimDups [x, y] 
	| (x == y) = [x]
	| otherwise = [x, y] 
elimDups (x:y:xs) 
	| (x == y) = elimDups (y:xs)
	| otherwise = x:(elimDups (y:xs))

--functie care imparte o lista de duplicate consecutive in subliste
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

--functie care afiseaza o lista de perechi de forma 
--(N, E) - unde N este nr de cate ori este duplicat elementul E
encode l = map (\x -> (length x, head x)) (pack l)

--functie care duplica elementele unei liste 
dupli l = foldr(\x acc -> x:x:acc) [] l

--functie care replica elementele unei liste de un nr dat de ori
repli n l = foldl(\acc x -> acc ++ repli' n x) [] l where 
		repli' 0 _ = []
		repli' n x = x:(repli' (n-1) x)

--functie care elimina fiecare al n-lea numar
dropEvery _ [] = []
dropEvery n (x:xs) = dropEvery' n (x:xs) 1 where 
			dropEvery' _ [] _ = []
			dropEvery' n (x:xs) i = (if (n `mod` i == 0) then []
						else [x]) 
						++ (dropEvery' n xs (i+1))

--functie care face split la un index dat ca parametru
split n l = (take n l, drop n l) 

--functie care intoarce o subsecventa dintr-o lista
slice n k l = (drop (n-1) firstN) ++ (drop n firstK) where 
		firstN = (fst (split n l))
		firstK = (fst (split k l))

--rotirea unei liste cu n pozitii la stanga
rotate n l
	| (n >= 0) = (snd (split n l)) ++ (fst (split n l))
	| otherwise = (snd (split (length l + n) l)) ++ (fst (split (length l + n) l))

--functie care elimina al k-lea element dintr-o lista
removeAt k l = (l !! (k-1), rest l) where	
		rest l = foldr(\x acc -> if (l !! (k-1) == x) then acc else x:acc) [] l

--functie care insereaza un element la o anumita pozitie
insertAt e 1 l = e:l
insertAt e k (x:xs) = x:(insertAt e (k-1) xs)  

--functie care creeaza o lista cu elementele intr-un anumit interval
range a b = slice a b [1..b] 

--functie care intoarce toate sublistele de lungime n
combinations 0 _ = [[]]
combinations n l = [l !! i : x | i <- [0..(length l - 1)], x <- combinations (n-1) (drop (i+1) l)]

---------------------------------------------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

--definire frunza
leaf x = Node x Empty Empty

--functie care intoarce numarul de noduri dintr-un arbore
numNodes Empty = 0 
numNodes (Node k Empty Empty) = 1
numNodes (Node k l r) = 1 + (numNodes l) + (numNodes r)

--functie care verifica daca doi arbori sunt balanced
isBalance Empty Empty = True
isBalance Empty _ = False
isBalance _ Empty = False
isBalance t1@(Node k l r) t2@(Node k' l' r') 
	| abs (numNodes t1 - numNodes t2) <= 1 = True
	| otherwise = False

tree1 = Node 1 (Node 2 Empty (Node 4 Empty Empty)) (Node 2 Empty Empty)
tree2 = Node 3 (Node 4 (Node 3 Empty Empty) Empty) (Node 5 Empty Empty)

--functie care verifica daca un arbore este "oglinditul" celuilalt
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Node k l r) (Node k' l' r') = (mirror l r') && (mirror r l')

--functie care verifica daca un arbore este simetric
symmetric Empty = True
symmetric (Node k l r) = mirror l r 

tree3 = Node 'x' (Node 'x' Empty Empty) Empty
tree4 = Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty)

--functie care insereaza ordonat un nod intr-un BST
insertOrd x Empty = Node x Empty Empty 

insertOrd x t@(Node k l r)
	| (x == k) = t
	| (x < k) = Node k (insertOrd x l) r
	| otherwise = Node k l (insertOrd x r)

--functie care construieste un BST dintr-o lista
construct l = foldl (\acc x -> insertOrd x acc) Empty l

--functie care numara frunzele dintr-un arbore
countLeaves Empty = 0
countLeaves (Node x Empty Empty) = 1
countLeaves (Node k l r) = (countLeaves l) + (countLeaves r)

--functie care colecteaza frunzele intr-o lista 
leaves Empty = []
leaves (Node x Empty Empty) = [x]
leaves (Node k l r) = (leaves l) ++ (leaves r)

--functie care intoarce nodurile interne sub forma de lista
internals Empty = []
internals (Node k Empty Empty) = []
internals (Node k l r) = k:(internals l) ++ (internals r) 

--functie care intoarce nodurile de pe un anumit nivel 
atLevel _ Empty = []
atLevel n (Node k l r) 
	| (n == 1) = [k]
	| n > 1 = (atLevel (n-1) l) ++ (atLevel (n-1) r)
	| otherwise = []

--arbore multicai
data MTree a = MNode a [MTree a] deriving (Show, Eq)

--functie care intoarce numarul de noduri ale unui arbore multicai
nnodes (MNode x t) = 1 + length t 

tree5 = MNode 'a' [MNode 'b' []]

--functie care calculeaza suma distantelor de la root la fiecare nod
internalPath = internalAux 0 where 
			internalAux d (MNode _ l) = d + sum (map (internalAux (d+1)) l)



