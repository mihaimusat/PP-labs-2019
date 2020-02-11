
f1 [] = []
f1 (x:xs) = [x]:(f1 xs)

--suma primelor k nr naturale -> v1
nats = 1:(map (+1) nats)
firstK k = take k
suma acc [] = acc
suma acc (x:xs) = suma (acc + x) xs

--suma primelor k nr naturale -> v2
sumK k = accumulate k 0 where
		accumulate 0 acc = acc
		accumulate k acc = accumulate (k - 1) (acc + k)

--program v1
aduna x = foldl (+) x [0..9]

--program v2
increaseNumber x = addToX x 0 where --0 este val initiala a contorului
			addToX x 10 = x --cand am ajuns cu contorul la 10, intorc x
			addToX x acc = addToX (acc + x) (acc + 1) --pasul de incrementare pentru x si contor

data Boolean = Adevarat | Fals

instance Show Boolean where 
	show Adevarat = show True
	show Fals = show False

instance Eq Boolean where 
	Adevarat == Adevarat = True
	Fals == Fals = True
	_ == _ = False

myAnd :: Boolean -> Boolean -> Boolean
myAnd Adevarat Adevarat = Adevarat
myAnd _ _ = Fals

data A a = B a String | C

--functie care elimina duplicatele consecutive dintr-o lista
elimina [] = []
elimina [x] = [x]
elimina (x:y:xs)
	| x == y = elimina (y:xs)
	| otherwise = x:(elimina (y:xs))

--functie care realizeaza interclasarea a doua liste sortate 
interclasare [] [] = []
interclasare [x] [] = [x]
interclasare [] [x] = [x]
interclasare [x] [y]
	| x < y = [x, y]
	| otherwise = [y, x]
interclasare (x:xs) (y:ys)
	| x < y = x:y:(interclasare xs ys)
	| otherwise = y:x:(interclasare xs ys)

--functie care scoate spatiile dintr-un cuvant
filterSpace :: String -> String
filterSpace s = filter (\x -> x /= ' ') s

--functie care inlocuieste toate spatiile cu newline
replaceSpace "" = ""
replaceSpace (' ':xs) = "\n" ++ replaceSpace xs
replaceSpace (x:xs) = x:(replaceSpace xs)

find7 [] = False
find7 l = elem 7 l

makeList [] n = [[n]]
makeList (x:xs) n = [x]:(makeList xs n)

data Pair a b = Pair a b

instance (Show a, Show b) => Show (Pair a b) where 
	show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"

data Parent a = Mother a | Father a | Grandparent (Parent a) deriving Show

instance (Eq a) => Eq (Parent a) where
	Grandparent x == Grandparent y = x == y
	Mother x == Mother y = x == y
	Father x == Father y = x == y
	_ == _ = False 

--functie care ia un element si o lista si numara de cate ori apare un element
countApp e (x:xs) = foldr (\x y -> if (elem x xs) && (x==e) then (y+1) else y) 0 (x:xs)

--functie care intoarce elementele din lista care apar de cel mult 4 ori
apare4 [] = []
apare4 [x] = [x]
apare4 (x:xs) = foldr (\e y -> if countApp e (x:xs) > 4 then y else e:y) [] (x:xs) 

prodPairs l = foldr (\x acc -> ((fst x)*(snd x)):acc) [] l

--functie care verifica ca o lista este palindrom 

pali l = check l (reverse l) where 
		check [] [] = True
		check (x:xs) (y:ys) 
			| (x==y) = check xs ys
			| otherwise = False

--functie care afiseaza a n-a linie dintr-o matrice 
nthLine n mat = head (drop (n-1) mat)

--functie care realizeaza transpunerea unei matrici
transpose ([]:_) = []
transpose mat = (map head mat):transpose(map tail mat)

--functia reverse 
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--functie care elimina toate nr impare dintr-o lista 
elimOdd [] = [] 
elimOdd (x:xs) 
	| odd x == True = elimOdd xs
	| otherwise = x:(elimOdd xs)

--functie care realizeaza concatenarea inversa a doua liste
invConcat [] [] = [] 
invConcat [x] [] = [x]
invConcat [] [x] = [x]
invConcat l1 l2 = l2 ++ l1

data Natural = Zero | Succ Natural

--functie care intoarce primele k nr naturale
getNumbers :: Integer -> [Natural]
getNumbers k = if (k<0) then []
	       else getNumbers (k-1) ++ [(aux k)] where 
			aux 0 = Zero
			aux k = Succ (aux (k-1))

--functie care converteste un Natural intr-un intreg
convert Zero = 0 
convert (Succ x) = 1 + convert x

instance Show Natural where 
	show nat = show (convert nat)

--functie care intoarce predecesorul pentru un Natural
getPred :: Natural -> Natural
getPred Zero = Zero 

data Person = Adult Integer | Young Integer deriving Show

getAge :: Person -> Integer
getAge (Adult age) = age
getAge (Young age) = age

--functie care verifica daca doua liste sunt egale
sameLists [] [] = True
sameLists (x:xs) (y:ys)
	| (x==y) = sameLists xs ys
	| otherwise = False

ppf = \f -> filter f "Text"
f x = if ( (x/='t') && (x/='T')) then True else False

e x = map (\y -> (1,'2'):y) x

data PP a = One (PP a) | Two a (PP a)

data A1 a = B1 a | C1 

--functie care sa aiba urmatorul comportament: [1, 2, 3] => [3,3,2,2,1,1]
dupInv l = foldl (\acc x -> x:x:acc) [] l

--functie care sa intoarca al k-lea element par din lista
kEven k l = head (drop (k-1) (filter even l))

--TDA pentru arbore care poate avea oricate subnoduri
data Tree a = Empty | Node [(Tree a)]

--functie care intoarce divizorii unui numar
divizors n = filter (\x -> n `mod` x == 0) [1..div n 2]

--functie care verifica daca un numar este perfect
isPerfect n = n == sum (divizors n)

--sirul numerelor perfecte
perfects = filter isPerfect nats

--generator de sir pe care aplic f si cu valoarea de start a0
build f a0 = a0:(map f (build f a0))

data Arbore a = Frunza | Nod a (Arbore a) (Arbore a) 

--foldr pentru arbore
foldrT f acc Frunza = acc
foldrT f acc (Nod k l r) = f k (foldrT f (foldrT f acc r) l)

--verificare daca un element exista intr-o lista infinita
findElem e l = foldr(\x acc -> if (elem x l) && (x==e) then True else acc) False l

--produsul cartezian a doua multimi (liste)
prodCart [] [] = []
prodCart l1 l2 = [(x, y) | x <- l1, y <- l2] 

--functie care pastreaza doar elementele care apar de mai multe ori in lista
multiApp [] = [] 
multiApp (x:xs) = foldr (\e acc -> if (countApp e (x:xs) > 1) then e:acc else acc) [] (x:xs)

--sirul puterilor lui 2
powersOfTwo = 1:(zipWith (+) powersOfTwo powersOfTwo)

--data Prog a = Val a | List [Prog a] | Cons (Prog a) (Prog a) | App (Prog a) (Prog a) deriving (Show, Read)

----------------------------------------------------------------------------

data Set a = Set (a->Bool)

--functie care face reuniunea a doua multimi
union (Set s1) (Set s2) = Set or


data List a = Void | Cons a (List a) 

instance (Eq a) => Eq (List a) where 
	Void == Void = True
	Cons a as == Cons b bs = (a==b) && (as == bs)
	_ == _ = False

instance (Show a) => Show (List a) where 
	show l = "[" ++ showAux l ++ "]" where
		showAux Void = ""
		showAux (Cons x Void) = show x
		showAux (Cons x xs) = show x ++ "," ++ showAux xs
		


