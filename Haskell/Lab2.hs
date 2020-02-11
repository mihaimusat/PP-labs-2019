{-
 - Paradigme de Programare CB
 - Laborator 2
 -}
import Control.Exception (assert)

myFilter _ [] = []
myFilter f (x:xs) = if f x 
			then x:(myFilter f xs)
			else (myFilter f xs)

evenElems = myFilter even
greaterThan10 = myFilter (>10)

chains :: (Integral a) => a -> [a]
chains 1 = [1]
chains n 
	| even n = n:chains(n `div` 2)
	| otherwise = n:chains(n*3 + 1)

countChains = length (filter isLong (map chains [1..100]))
		where isLong xs = length xs > 15

------------------------------------------------------------

-- 1
subFrom :: Int -> [Int] -> Int
--subFrom = undefined

subFrom = foldl (-)

-- 2
reverseFold :: (Eq a) => [a] -> [a]
--reverseFold = undefined

reverseFold = foldl (flip (:)) []

-- 3
prefix123 :: [Int] -> [Int]
--prefix123 = undefined

prefix123 l = [1, 2, 3] ++ l

-- 4.a
elimFold :: String -> String
--elimFold = undefined

elimFold = foldr (\x acc -> if x =='a' then acc else x:acc) []

-- 4.b
elimFilter :: String -> String
--elimFilter = undefined

elimFilter = filter (\x -> x/='a') 

-- 5
concatStr :: [String] -> String
--concatStr = undefined

concatStr = foldr (\x acc -> x ++ acc) []

-- 6
countDiff :: String -> String -> Int
--countDiff = undefined

countDiff s1 s2 = foldr (\x acc -> if x==True then (acc + 1) else acc) 0 (zipWith (\x y -> x/=y) s1 s2) 

-- 7.a
doubleF :: (a -> a) -> a -> a
--doubleF = undefined

doubleF f x = (f.f) x

-- 7.b
inverseOp :: (a->b->c) -> (b->a->c)
--inverseOp = undefined

inverseOp = flip

-- 8.a
foldl' :: (a -> b -> a) -> a -> [b] -> a
--foldl' = undefined

foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- 8.b
foldr' :: (a -> b -> b) -> b -> [a] -> b
--foldr' = undefined

foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- 8.c
map' :: (a -> b) -> [a] -> [b]
--map' = undefined

map' _ [] = []
map' f (x:xs) = f x : (map' f xs)

-- 8.d
filter' :: (a -> Bool) -> [a] -> [a]
--filter' = undefined

filter' _ [] = []
filter' f (x:xs)
	| f x = x:(filter' f xs)
	| otherwise = filter' f xs

-- 8.e
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith' = undefined

--zipWith' _ [] [] = []
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)  

-- 9
comp' :: (a -> b) -> (c -> a) -> c -> b
--comp' = undefined

comp' f g = (f.g)

--10. Implementati folosind foldr/foldl 
--1) map 
--2) filter

map'' f = foldr (\x acc -> f x : acc) [] 
filter'' f = foldl (\acc x -> if f x then acc ++ [x] else acc) [] 

-- 11
process :: String -> Int -> String
--process = undefined

group' :: String -> [String]
group' [] = []
group' l@(x:xs) = same : group' remainder
            where
                (same, remainder) = span (== x) l

countEl :: [String] -> [(String, String)]
countEl = map (\x -> ((show . length) x, [head x]))

concatTup :: [(String, String)] -> String
concatTup = concatMap (\(x,y) -> x ++ y)

process s k 
	| k <= 0 = s
	| otherwise = process (concatTup (countEl (group' s))) (k-1)


test1 = [
        assert (subFrom 0 [] == 0) "Success for subFrom 0 []",
        assert (subFrom 5 [1..3] == -1) "Success for subFrom 5 [1..3]",
        assert (subFrom 10 [-5..0] == 25) "Success for subFrom 10 [-5..0]",
        assert (subFrom 0 [-100..0] == 5050) "Success for subFrom 0 [0..10]"
    ]

test2 = [
        assert (reverseFold [1..100] == [100,99..1]) "Success for reverseFold [1..100]",
        assert (reverseFold [10,12..100] == [100,98..10]) "Success for reverseFold [10,12..100]",
        assert (reverseFold [30,40..10000] == [10000,9990..30]) "Success for reverseFold [30,40..10000]"
    ]

test3 = [
        assert (prefix123 [] == [1,2,3]) "Success for prefix []",
        assert (prefix123 [1,2,3] == [1,2,3,1,2,3]) "Success for prefix [1,2,3]",
        assert (prefix123 [5,6,7] == [1,2,3,5,6,7]) "Success for prefix [5,6,7]",
        assert (prefix123 [10,11,3] == [1,2,3,10,11,3]) "Success for prefix [10,11,3]"
    ]

test4a = [
        assert (elimFold "" == "") "Success for elimFold \"\"",
        assert (elimFold ['b'..'z'] == ['b'..'z']) "Success for elimFold ['b'..'z']",
        assert (elimFold "aaaa" == "") "Success for elimFold \"aaaa\"",
        assert (elimFold "Ana are mere si pere" == "An re mere si pere") "Success for elimFold \"Ana are mere si pere\""
    ]

test4b = [
        assert (elimFilter "" == "") "Success for elimFilter \"\"",
        assert (elimFilter ['b'..'z'] == ['b'..'z']) "Success for elimFilter ['b'..'z']",
        assert (elimFilter "aaaa" == "") "Success for elimFilter \"aaaa\"",
        assert (elimFilter "Ana are mere si pere" == "An re mere si pere") "Success for elimFilter \"Ana are mere si pere\""
    ]

test5 = [
        assert (concatStr ["", "", ""] == "") "Success for concatStr ['', '', '']",
        assert (concatStr ["a","b","c","d"] == "abcd") "Success for concatStr [\"a\",\"b\",\"c\",\"d\"]",
        assert (concatStr ["1","","1",""] == "11") "Success for concatStr [\"1\", \"\", \"1\", \"\"]",
        assert (concatStr ["Ana","are","mere","si","pere"] == "Anaaremeresipere") "Success for concatStr [\"Ana\",\"are\",\"mere\",\"si\",\"pere\"]"
    ]

test6 = [
        assert (countDiff "" "" == 0) "Success for countDiff \"\" \"\"",
        assert (countDiff "abc" "cba" == 2) "Success for countDiff \"abc\" \"cba\"",
        assert (countDiff ['a'..'z'] ['a'..'z'] == 0) "Success for countDiff ['a'..'z'] ['a'..'z']",
        assert (countDiff "PP-PC-PA" "PP-PA-PC" == 2) "Success for countDiff \"PP-PC-PA\" \"PP-PA-PC\""
    ]

test7a = [
        assert (doubleF (\x->x*2+3) 0 == 9) "Success for doubleF (x->x*2+3) 0",
        assert (doubleF (3 +) 5 == 11) "Success for doubleF (3 +)",
        assert (doubleF (5 *) 0 == 0) "Success for doubleF (5 *)",
        assert (doubleF (10 /) 5 == 5) "Success for doubleF (10 -)"
    ]

test7b = [
        assert (inverseOp (-) 2 3 == 1) "Success for inverseOp - 2 3",
        assert (inverseOp (+) 5 3 == 8) "Success for inverseOp (+) 3",
        assert (inverseOp (*) 1 10 == 10) "Success for inverseOp (*) 1 10",
        assert (inverseOp (/) 5 5 == 1) "Success for inverseOp (/) 5 5"
    ]

test8a = [
        assert (foldl' (++) [] [[1],[2],[3]] == [1,2,3]) "Success for foldl' (++) [] [[1],[2],[3]]",
        assert (foldl' (+) 0 [1,2,3] == 6) "Success for foldl' (+) 0 [1,2,3]",
        assert (foldl' (*) 5 [1..5] == 600) "Success for foldl' (*) 5 [1..5]",
        assert (foldl' (/) 600 [1..5] == 5) "Success for foldl' (/) 600 [1..5]"
    ]

test8b = [
        assert (foldr' (++) [] [[1],[2],[3]] == [1,2,3]) "Success for foldr' (:) [] [1,2,3]",
        assert (foldr' (+) 0 [1,2,3] == 6) "Success for foldr' (+) 0 [1,2,3]",
        assert (foldr' (*) 5 [1..5] == 600) "Success for foldr' (*) 5 [1..5]",
        assert (foldr' (:) [] [1..5] == [1..5]) "Success for foldr' (:) [] [1..5]"
    ]

test8c = [
        assert (map' (+2) [1,2,3] == [3,4,5]) "Success for map' (:) [] [1,2,3]",
        assert (map' ([1,2,3] ++)  [[1],[2],[3]] == [[1,2,3,1],[1,2,3,2],[1,2,3,3]]) "Success for map' (++ [1,2,3]) [[1],[2],[3]]",
        assert (map' (*10) [1..100] == [10,20..1000]) "Success for map' (*10) [1..100]",
        assert (map' ((+3).(*2)) [1..10] == [5,7..23]) "Success for map' ((+3).(*2)) [1..10]"
    ]

test8d = [
        assert (filter' odd [1..100] == [1,3..100]) "Success for filter' odd [1..100]",
        assert (filter' (\x->mod x 5 == 0) [1..100] == [5,10..100]) "Success for filter' (x->mod x 5 == 0) [1..100]",
        assert (filter' (\x->mod (fst x) 3 == 0) [(x,y) | x<-[1..100], y<-[1..100]] == [(x,y) | x<-[3,6..100], y<-[1..100]]) "Success for filter' (x->mod (fst x) 3 == 0) [(x,y) | x<-[1..100], y<-[1..100]]",
        assert (filter' (\x->length x < 3) [[1],[1..5],[1..10],[1..20]] == [[1]]) "Success for filter' (x->length x < 3) [[1],[1..5],[1..10],[1..20]]"
    ]

test8e = [
        assert (zipWith' (+) [1..100] [1..100] == [2,4..200]) "Success for zipWith' (+) [1..100] [1..100]",
        assert (zipWith' (-) [1..100] [1..300] == replicate 100 0) "Success for zipWith' (-) [1..100] [1..300]",
        assert (zipWith' (*) [1,2,3] [5,6,7] == [5,12,21]) "Success for zipWith' (*) [1,2,3] [5,6,7]",
        assert (zipWith' (\x y->(x+y)*(y-x)) [1,2,3] [4,5,6] == [15,21,27]) "Success for zipWith' (x y->(x+y)*(y-x)) [1,2,3] [4,5,6]"
    ]

test9 = [
        assert (comp' (3+) (4-) 3 == 4) "Success for comp' (3+) (4-) 3",
        assert (comp' ([1,2,3] ++) ([5,6,7] ++) [0] == [1,2,3,5,6,7,0]) "Success for comp' ([1,2,3] ++) ([5,6,7] ++) [0]",
        assert (comp' sum ([1,1,1] ++) [2] == 5) "Success for comp' (foldl (+) 0) ([1,1,1] ++) [2]",
        assert (comp' (filter odd) (map (+2)) [1..100] == [3,5..102]) "Success for comp' (filter odd) (map (+2)) [1..100]"
    ]

test10 = [
        assert (process [] (-3) == []) "Success for process [] -3",
        assert (process "111" 2 == "1311") "Success for process \"111\" 2",
        assert (process "534" 2 == "111511131114") "Success for process \"534\" 2",
        assert (process "1" 10 == "11131221133112132113212221") "Success for process \"1\" 10"
    ]

allTests = [test1,test2,test3,test4a,test4b,test5,test6,test7a,test7b,test8a,test8b,test8c,test8d,test8e,test9,test10]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test
