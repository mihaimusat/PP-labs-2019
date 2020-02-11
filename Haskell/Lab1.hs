{-
 - Paradigme de Programare CB
 - Laborator 1
 -}

import Control.Exception (assert)

--prima functie in Haskell -> ocw
myFunc :: Int -> Int -> Int
myFunc x y = abs (2 * max x y)

--suma elementelor unei liste
mySum [] = 0
mySum (x:xs) = x + mySum xs

--varianta tail-recursive a sumei elementelor unei liste
mySum2 l = let sumAux [] acc = acc
	       sumAux (x:xs) acc = sumAux xs (x+acc)
	   in sumAux l 0

----------------------------------------------------------

-- 1.a
fact :: Int -> Int
--fact = undefined

fact 0 = 1 
fact n = n * fact (n - 1)

-- 1.b
factTailR :: Int -> Int
--factTailR  = undefined

factTailR n = let factAux 0 acc = acc 
		  factAux n acc = factAux (n - 1) n * acc
	      in factAux n 1

-- 2.a
fib :: Int -> Int
--fib = undefined

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 2.b
fibTailR :: Int -> Int
--fibTailR = undefined

fibTailR n = let fibAux 0 _ acc = acc
		 fibAux n x acc = fibAux (n - 1) acc (acc + x)
	     in fibAux n 0 1

-- 3.a
cat :: [a] -> [a] -> [a]
--cat = undefined

cat [] l = l
cat l [] = l
cat (x:xs) l = x:(cat xs l)

-- 3.b
inv :: [a] -> [a]
--inv = undefined

inv [] = [] 
inv (x:xs) = (inv xs) ++ [x]

-- 4.a merge sort
mergeSort :: (Ord a) => [a] -> [a]
--mergeSort = undefined

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l@(x:xs) = merge (mergeSort first) (mergeSort second)
		     where first = take (div (length l) 2) l
			   second = drop (div (length l) 2) l
			   merge [] t = t
		  	   merge t [] = t
			   merge (a:as) (b:bs) 
				| a < b = a:(merge as (b:bs))
				| otherwise = b:(merge (a:as) bs)


-- 4.b insert sort
insertSort :: (Ord a) => [a] -> [a]
--insertSort = undefined

insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert x (insertSort xs) 
		    where insert x [] = [x]
			  insert x (y:ys) 
				| x < y = x:y:ys
				| otherwise = y:(insert x ys)

-- 4.c quick sort
quickSort :: (Ord a) => [a] -> [a]
--quickSort = undefined

quickSort [] = []
quickSort (x:xs) = quickSort (smallerThan x) ++ [x] ++ quickSort (greaterThan x)
		   where smallerThan x = [p | p <- xs, p < x]
			 greaterThan x = [p | p <- xs, p >= x]

-- 5
binarySearch :: (Ord a) => a -> [a] -> Bool
--binarySearch = undefined

binarySearch _ [] = False
binarySearch x l 
	| x == mid = True
	| x < mid = binarySearch x low
	| otherwise = binarySearch x high
	where index = div (length l) 2
	      (low, mid:high) = splitAt index l 

test1a = [
        assert (fact 0 == 1) "Success fact 0",
        assert (fact 1 == 1) "Success fact 1",
        assert (fact 2 == 2) "Success fact 2",
        assert (fact 10 == 3628800) "Success fact 10"
    ]

test1b = [ 
        assert (factTailR 0 == 1) "Success factTailR 0",
        assert (factTailR 1 == 1) "Success factTailR 1",
        assert (factTailR 2 == 2) "Success factTailR 2",
        assert (factTailR 10 == 3628800) "Success factTailR 10"
    ]

test2a = [ 
        assert (fib 0 == 1) "Success fib 0",
        assert (fib 1 == 1) "Success fib 1",
        assert (fib 10 == 89) "Success fib 10",
        assert (fib 31 == 2178309) "Success fib 31"
    ]

test2b = [ 
        assert (fibTailR 0 == 1) "Success fibTailR 0",
        assert (fibTailR 1 == 1) "Success fibTailR 1",
        assert (fibTailR 10 == 89) "Success fibTailR 10",
        assert (fibTailR 31 == 2178309) "Success fibTailR 31"
    ]

test3a = [
        assert (cat [] [1,2,3] == [1,2,3]) "Success for cat [] [1,2,3]",
        assert (cat [1,2,3] [] == [1,2,3]) "Success for cat [1,2,3] []",
        assert (cat [1,2,3] [-1,-2,-3] == [1,2,3,-1,-2,-3]) "Success for cat [1,2,3] [-1,-2,-3]",
        assert (cat ['a','b','c'] ['d','e','f'] == "abcdef") "Success for cat ['a','b','c'] ['d','e','f']",
        assert (cat "PP" "CB" == "PPCB") "Success for cat \"PP\" \"CB\""
        --- Q: De ce ghc nu permite sa facem cat [] []?
    ]

test3b = [
        assert (inv [1,2,3] == [3,2,1]) "Succes for inv [1,2,3]",
        assert (inv [1,5,6,10] == [10,6,5,1]) "Success for inv [1,5,6,10]",
        assert (inv ["great","work"] == ["work","great"]) "Success for inv \"great\" \"work\"",
        assert (inv [[2,3], [4,5]] == [[4,5], [2,3]]) "Success for inv [[2,3], [4,5]]"
    ]

test4a = [
        assert (mergeSort [3,2,1] == [1,2,3]) "Success for mergeSort [3,2,1]",
        assert (mergeSort [5,3,-3,4] == [-3,3,4,5]) "Success for mergeSort [5,3,-3,4]",
        assert (mergeSort [10,-3,5,5,-1,42] == [-3,-1,5,5,10,42]) "Success for mergeSort [10,-3,5,5,-1,42]",
        assert (mergeSort [-5,10,-3,2,5,10,-1,4] == [-5,-3,-1,2,4,5,10,10]) "Success for mergeSort [-5,10,-3,2,5,10,-1,4]"
    ]

test4b = [
        assert (insertSort [3,2,1] == [1,2,3]) "Success for insertSort [3,2,1]",
        assert (insertSort [5,3,-3,4] == [-3,3,4,5]) "Success for insertSort [5,3,-3,4]",
        assert (insertSort [10,-3,5,5,-1,42] == [-3,-1,5,5,10,42]) "Success for insertSort [10,-3,5,5,-1,42]",
        assert (insertSort [-5,10,-3,2,5,10,-1,4] == [-5,-3,-1,2,4,5,10,10]) "Success for insertSort [-5,10,-3,2,5,10,-1,4]"
    ]

test4c = [
        assert (quickSort [3,2,1] == [1,2,3]) "Success for quickSort [3,2,1]",
        assert (quickSort [5,3,-3,4] == [-3,3,4,5]) "Success for quickSort [5,3,-3,4]",
        assert (quickSort [10,-3,5,5,-1,42] == [-3,-1,5,5,10,42]) "Success for quickSort [10,-3,5,5,-1,42]",
        assert (quickSort [-5,10,-3,2,5,10,-1,4] == [-5,-3,-1,2,4,5,10,10]) "Success for quickSort [-5,10,-3,2,5,10,-1,4]"
    ]

test5 = [
        assert (binarySearch "PP" ["PA","PC","PP"] == True) "Success for binarySearch \"PP\" [\"PA\",\"PC\",\"PP\"]",
        assert (binarySearch 5 [1,2,3,5,100] == True) "Success for binarySearch 5 [1,2,3,5,100]",
        assert (binarySearch 10 [1,2,3,4,5,6] == False) "Success for binarySearch 10 [1,2,3,4,5,6]",
        assert (binarySearch 5 [5,6,7,8,9,10] == True) "Success for binarySearch 5 [5,6,7,8,9,10]",
        assert (binarySearch 10 [5,6,7,8,9,10] == True) "Success for binarySearch 10 [5,6,7,8,9,10]"
    ]

allTests = [test1a,test1b,test2a,test2b,test3a,test3b,test4a,test4b,test4c,test5]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test

----------------------------------------------------------------------------

{- Pattern Matching
 -}

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs  

max' :: (Ord a) => a->a->a
max' a b
	| a > b = a
	| otherwise = b

calcBmis :: (RealFloat a) => a->a->String
calcBmis weight height
	| bmi <= 18.0 = "Skinny"
	| bmi <= 25.0 = "Normal"
	| bmi <= 30.0 = "Fatty"
	| otherwise = "Whale"
	where bmi = weight / height^2
head' :: [a] -> a
head' xs = case xs of [] -> error "This is an empty list !"
		      (x:_) -> x

maximum' :: (Ord a) => [a]->a
maximum' [] = error "This is an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x 
	| n <= 0 = []
	| otherwise = x:(replicate' (n - 1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _ 
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' p (x:xs) 
	| x==p = True
	| otherwise = elem' p xs

