--functie care realizeaza maximul dintr-o lista
maxList [] = 0
maxList (x:xs)
	| x >= (maxList xs) = x
	| otherwise = maxList xs 

--functie care determina de cate ori apare un element in lista
countApp _ [] = 0
countApp e (x:xs)
	| (x==e) = 1 + countApp e xs 
	| otherwise = countApp e xs

--functie care intoarce lista elementelor care apar de mai mult de 4 ori in input
apare4 [] = [] 
apare4 (x:xs) 
	| countApp x (x:xs) > 4 = x:(apare4 xs)
	| otherwise = apare4 xs

--functie care primeste o lista de perechi si inmulteste componentele fiecarei perechi
--ex: [(1,2),(3,4),(5,6)] -> [2,12,20]
prodPairs l = foldr(\x acc -> ((fst x) * (snd x)):acc) [] l


--functie care inverseaza o lista
rev [] = []
rev [x] = [x] 
rev (x:xs) = rev xs ++ [x]

--functie care verifica ca doua liste sunt egale
eqLists [] [] = True
eqLists [] l = False
eqLists l [] = False
eqLists (x:xs) (y:ys) = (x==y) && (eqLists xs ys)

--functie care verifica ca o lista este palindrom
pali l1 
	| eqLists l1 (rev l1) = True
	| otherwise = False 

--functie care extrage a n-a linie dintr-o matrice
line n mat = head (drop (n-1) mat)

--functie care realizeaza transpunerea unei matrici
transpose ([]:_) = []
transpose mat = (map head mat):(transpose (map tail mat))

--functie care elimina numerele impare dintr-o lista
getEven l = filter even l

--functie care face suma pe linii a unei matrici
sumLin l = foldr(\x acc -> (foldl (+) 0 x):acc) [] l

--functie care intoarce ultimul element impar din lista
lastOdd l = head(reverse(filter odd l))


--tip de date pentru lambda expresii
data Lambda = Var String | Expr String Lambda | App Lambda Lambda

instance Show Lambda where
    show (Var v) = v
    show (Expr x b) = "λ" ++ x ++ "." ++ show b
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

--tip de date care modeleaza formule din logica
data Prop = Prop String |
	    Not Prop | 
	    And Prop Prop | 
	    Or Prop Prop | 
	    Impl Prop Prop | 
	    Equiv Prop Prop | 
            ForAll String Prop | 
	    Exists String Prop deriving (Eq, Ord)

instance Show Prop where 
	show (Prop p) = p
	show (Not p) = "~" ++ (show p)
	show (And p q) = (show p) ++ "^" ++ (show q) 
	show (Or p q) = (show p) ++ "v" ++ (show q)
	show (Impl p q) = (show p) ++ "->" ++ (show q)
	show (Equiv p q) = (show p) ++ "<->" ++ (show q)
	show (ForAll var p) = "forall" ++ (show var) ++ (show p)
	show (Exists var p) = "exists" ++ (show var) ++ (show p)

--functie care sa aiba urmatorul comportament
--f([1,2,3]) = [3,3,2,2,1,1]
duplica l = foldl(\acc x -> x:x:acc) [] l 

--functie care sa intoarca al k-lea element par din lista
kEven k l = head (drop (k-1) (filter even l))

--definire TDA pentru un arbore care poate avea oricate subnoduri
--data Tree a = Nil | Node a ([Tree a])

--functie care elimina duplicatele din lista
remDup [] = [] 
remDup (x:xs) 
	| countApp x (x:xs) > 1 = remDup xs
	| otherwise = x:(remDup xs)

--reuniunea a doua multimi fara duplicate
reunion l [] = l
reunion [] l = l
reunion (x:xs) l 
	| elem x l = reunion xs l 
	| otherwise = x:(reunion xs l)

--definire TDA pentru coada
{-
data Queue a = Empty | Coada a (Queue a) deriving (Eq, Show)

--operatii pe coada
enqueue x Empty = Coada x Empty
enqueue x (Coada y ys) = Coada y (enqueue x ys)

dequeue Empty = Nothing
dequeue (Coada x xs) = Just xs
-}

--functie care verifica daca o lista este sortata
isSorted [] = True
isSorted [x] = True
isSorted (x:y:ys) = (x <= y) && isSorted (y:ys)
 
--definire TDA pentru stiva
data Stack a = Void | Stiva a (Stack a) deriving (Eq, Show) 

--operatii pe stiva
push x Void = Stiva x Void
push x (Stiva y ys) = Stiva x (push y ys)

pop Void = Nothing
pop (Stiva x xs) = Just xs

--definire sirul patratelor perfecte 
patrate = zipWith (*) nats nats where 
		nats = 1:(map(+1) nats) 

--definire TDA pentru arbore normal
data Tree a = Nil | Node a (Tree a) (Tree a)

foldT f acc Nil = acc
foldT f acc (Node k l r) = f k (foldT f (foldT f acc r) l) 

--functie care compune o functie cu ea insasi de oricate ori
compositions f = f:(map (f.) (compositions f))

--functie care verifica daca un element exista intr-o lista cu foldr
exists e l = foldr(\x acc -> if(x==e) then True else acc) False l

--functie care realizeaza produsul cartezian a doua liste
cart l1 l2 = [(x, y) | x <- l1, y <- l2] 

--definire puteri ale lui 2 ca flux
powers2 = 1:(zipWith (+) powers2 powers2)

--functie care sterge elementele care apar de mai mult de o data in lista
delApp l = foldr(\x acc -> if countApp x l > 1 then acc else x:acc) [] l 

--functie care pastreaza elementele care apar mai mult de o data in lista
keepDup [] = []
keepDup (x:xs) 
	| elem x xs = x:(keepDup (filter (/=x) xs))
	| otherwise = keepDup xs

--functie care realizeaza diferenta intre doua multimi
setDiff a b = foldr(\x acc -> if (elem x a == True) && (elem x b == False) then x:acc else acc) [] a

--functie care realizeaza intersectia intre doua multimi
setIntersect a b = foldr(\x acc -> if (elem x a) && (elem x b) then x:acc else acc) [] a

--definire clasa Ended 
class Ended t where 
	frontEnd :: t v -> v 
	backEnd :: t v -> v

--instantiere clasa pentru tipul lista
instance Ended [] where 
	frontEnd = head
	backEnd = last

--tipul pereche cu componente de acelasi tip 
data Pair a = MakePair a a deriving Show

instance Ended Pair where 
	frontEnd (MakePair x y) = x
	backEnd (MakePair x y) = y

--tipul triplu cu componente de acelasi tip 
data Triple a = T a a a

instance Ended Triple where 
	frontEnd (T x _ _) = x
	backEnd (T _ _ x) = x

--functie care aduna 3 fluxuri 
sumFlux f1 f2 f3 = (head f1 + head f2 + head f3):(sumFlux (tail f1) (tail f2) (tail f3))

--clasa care insumeaza elementele din structura de date primita ca argument
class Summable t where 
	suma :: (Num a) => t a -> a

instance Summable [] where
	suma [] = 0
	suma (x:xs) = x + suma xs

--definire lista imbricata cu elemente de tip a
data Nested a = Null | L [Nested a]

--introducerea tipului de date anterior in Summable
instance Summable Nested where
	suma Null = 0
	suma (L list) = foldl (+) 0 (map suma list)

--functie care face maximul dintre 3 numere
max3 x y z 
	| (x >= y) && (x >= z) = x
	| (y >= x) && (y >= z) = y
	| (z >= x) && (z >= y) = z

--functie care face maximul la fiecare index din cele 3 fluxuri 
maxFlux f1 f2 f3 = (max3 (head f1) (head f2) (head f3)):(maxFlux (tail f1) (tail f2) (tail f3))

class Addable t where 
	adunare :: (Num a) => t a -> t a -> t a

instance Addable [] where  
	adunare [] l = l
	adunare l [] = l
	adunare l1 l2 = zipWith (+) l1 l2

--instantiere clasa pentru tipul pereche
instance Addable Pair where 
	adunare (MakePair x y) (MakePair z t) = MakePair (x+z) (y+t)

--definire TDA pentru arbore in care fiecare nod poate avea oricati copii
data Arb a = Nod a [Arb a]

--functie care calculeaza inaltimea unui astfel de arbore
height (Nod k []) = 0
height (Nod k (x:xs)) = 1 + maxList (map height xs)

--functie care calculeaza numarul de frunze din arbore
leaves (Nod k []) = 1
leaves (Nod k (x:xs)) = sum (map leaves xs)

--fluxul listelor formate din primii 5 multipli ai fiecarui numar natural
mult5 = [take 5 [m | m <- [n..], mod m n == 0] | n <- [1..]]

--fluxul listelor de divizori pentru numerele naturale
divizors = [ [m | m <- [1..n], mod n m == 0] | n <- [1..]]

--definire TDA pentru multimap
type Multimap k v = [(k, [v])]

--definire operatie de lookup
lookup' :: (Eq k) => k -> (Multimap k v) -> Maybe [v]
lookup' _ [] = Nothing
lookup' key ((k, l):t)
	| (k == key) = Just l 
	| otherwise = lookup' key t 

--definirea operatiei de insert intr-un multimap
insert' :: (Eq k, Eq v) => k -> [v] -> (Multimap k v) -> (Multimap k v)
insert' k l [] = [(k, l)]
insert' key list [(k, v)]
	| (key == k) = [(k, v)] 
	| otherwise = (key, list):[(k, v)]

--definire operatie map pe multimap 
map' :: (a -> b) -> (Multimap k a) -> (Multimap k b)
map' f [] = []
map' f ((key, list):t) = (key, map f list):(map' f t)

--definire hash set
type Hashset v = [(Int, [v])]

--definire operatie values' care extrage valorile asociate cu un hash
values' :: Int -> (Hashset v) -> Maybe [v]
values' _ [] = Nothing
values' key ((k, l):t)
	| (k == key) = Just l 
	| otherwise = values' key t 

--definire operatie de insert in hash set
insert'' :: (Eq v) => Int -> [v] -> (Hashset v) -> (Hashset v)
insert'' k l [] = [(k, l)]
insert'' key list [(k, v)]
	| (key == k) = [(k, v)] 
	| otherwise = (key, list):[(k, v)]

--definire operatie de map pe hash set
map'' :: (a -> b) -> Hashset a -> Hashset b
map'' f [] = []
map'' f ((k, l):t) = (k, map f l):(map'' f t)

--definire tip de data student: nume_student [(nume_materie, nota_materie, credite_materie)]
--data Student = Student String [(String, Int, Int)] deriving (Eq, Show)

--introducere tip student in clasa Ord
{-
instance Ord Student where
	Student n materii1 < Student m materii2 = credite materii1 < credite materii2 where
		credite x = sum [c | (_, nota, c) <- x, nota >= 5]
-}
	
--definire tip de date pentru lista cu mai multe tipuri
data Val = I Int | C Char | P (Int, Char) deriving Show  
data MList = M [Val] deriving Show

--filtrare lista cu mai multe tipuri
filter' :: Char -> MList -> MList 
filter' c (M l) = M (filter (f c) l) where
			f 'i' (I _) = True
			f 'c' (C _) = True
			f 'p' (P (_, _)) = True
			f _ _ = False

--functie care intoarce lista caracterelor dintr-un MList
conv :: MList -> Maybe [Char]
conv (M ((C c):rest)) = case (conv (M rest)) of
				Just l -> Just (c:l)
				Nothing -> Nothing
conv (M []) = Just [] 
conv _ = Nothing

--definire alt tip de student
data Student = Student String [(String, Int)] deriving Show

--introducere in clasa Eq
instance Eq Student where 
	Student n lista1 == Student m lista2 = getStatus lista1 == getStatus lista2 where
		getStatus l = [f grade | ("PP", grade) <- l] where 
			f grade 
				| grade < 5 = 1
				| grade == 10 = 2
				| otherwise = 3


--implementare coada folosind 2 stive
data Queue a = Q [a] [a]

--verificare ca o stiva este goala
isEmpty :: Queue a -> Bool 
isEmpty (Q [] []) = True
isEmpty _ = False

--definire operatie de top 
top :: Queue a -> a
top (Q _ (x:xs)) = x
top (Q l []) = last l

--definire operatie de pop
del :: Queue a -> Queue a
del (Q s (x:xs)) = Q s xs
del (Q l []) = Q [] (tail (reverse l))

--definire inserare element in coada
ins :: a -> Queue a -> Queue a 
ins e (Q (x:xs) l) = Q (e:x:xs) l

--functie care face produsul scalar a doi vectori 
dotprod l1 l2 = foldl (+) 0 (zipWith (*) l1 l2)

--functie care selecteaza pentru fiecare element din l1 
--elementele mai mici din l2
getSmall e l = foldr(\x acc -> if (x<e) then x:acc else acc) [] l
makeSmall l1 l2 = foldr(\x acc -> (getSmall x l2):acc) [] l1

--functie care aplica elementul pe lista de functii
applyElem elem l = foldr(\x acc -> (x elem):acc) [] l

--functie care face split in doua liste
splitPairs [] = ([], [])
splitPairs ((x,y):t) = (x:(fst (splitPairs t)), y:(snd (splitPairs t)))

--functie care sa aiba comportament invers fata de splitPairs
groupPairs (x,y) = zipWith (\e1 e2 -> (e1, e2)) x y

--definire TDA pentru numere intregi
data IntNumber = Zero | Succ IntNumber | Prev IntNumber 

--introducere tip de numere intregi in clasa Show
instance Show IntNumber where
	show x = show (f x) where 
		f Zero = 0
		f (Succ m) = (f m) + 1
		f (Prev m) = (f m) - 1
		
