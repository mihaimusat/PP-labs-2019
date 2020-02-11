{-|
 - Paradigme de Programare CB
 - Laborator 4
 -}
module Lab4 where

import Control.Exception (assert)
import Data.List (sort)

-- 1. Binary Trees

-- Puteți ignora partea cu "deriving Eq". Pe scurt, ne permite să comparăm doi
-- arbori cu operatorul "==" (util pentru asserturi).
data BTree a = Nil | Node a (BTree a) (BTree a) deriving Eq

-- 1.a
foldrT :: (a -> b -> b) -> b -> BTree a -> b
--foldrT = undefined

--foldr f acc [] = acc
--foldr f acc (x:xs) = f x (foldr f acc xs)

foldrT f acc Nil = acc
foldrT f acc (Node k l r) = f k (foldrT f (foldrT f acc r) l)

--sa vedem cum merge si foldlT !!!
--foldl f acc [] = acc
--foldl f acc (x:xs) = foldl f (f acc x) xs

foldlT :: (b -> a -> b) -> b -> BTree a -> b
foldlT f acc Nil = acc
foldlT f acc (Node k l r) = foldlT f (f (foldlT f acc l) k) r

-- 1.b
mapT :: (a -> b) -> BTree a -> BTree b
--mapT = undefined

mapT _ Nil = Nil
mapT f (Node k l r) = Node (f k) (mapT f l) (mapT f r)

-- 1.c
zipWithT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
--zipWithT = undefined

zipWithT f Nil Nil = Nil 
zipWithT f Nil _ = Nil
zipWithT f _ Nil = Nil
zipWithT f (Node k l r) (Node k' l' r') = Node (f k k') (zipWithT f l l') (zipWithT f r r')


-- 2. Associative Lists
type Assoc k v = [(k, v)]

-- 2.a
insert :: (Eq k) => k -> v -> Assoc k v -> Assoc k v
--insert = undefined

insert key value [] = [(key, value)]
insert key value ((k,v):xs)
	| key == k = (key, value):xs
	| otherwise = (key, value):(k, v):xs

-- 2.b
getValue :: (Eq k) => k -> Assoc k v -> Maybe v
--getValue = undefined

getValue key [] = Nothing
getValue key ((k, v):xs) 
	| key == k = Just v
	| otherwise = getValue key xs

-- 2.c
removeKey :: (Eq k) => k -> Assoc k v -> Assoc k v
--removeKey = undefined

removeKey key [] = []
removeKey key ((k, v):xs)
	| key == k = xs
	| otherwise = (k, v):(removeKey key xs)

-- 2.d
getKeys :: Assoc k v -> [k]
--getKeys = undefined

getKeys [] = [] 
getKeys ((k, v):xs) = k:(getKeys xs)


-- 3. Extended natural numbers
data Extended = Infinity | Value Integer deriving Eq

-- 3.a
extSum :: Extended -> Extended -> Extended
--extSum = undefined

extSum (Value a) (Value b) = Value (a+b)
extSum Infinity Infinity = Infinity
extSum Infinity _ = Infinity
extSum _ Infinity = Infinity


-- 3.b
extDiv :: Extended -> Extended -> Extended
--extDiv = undefined

extDiv (Value x) Infinity = Value 0
extDiv Infinity (Value x) = Infinity
extDiv (Value x) (Value 0) = Infinity
extDiv (Value 0) (Value x) = Value 0
extDiv (Value a) (Value b) = Value (div a b) 

-- 3.c
extLess :: Extended -> Extended -> Bool
--extLess = undefined

extLess (Value x) Infinity = True
extLess Infinity _ = False
extLess (Value x) (Value y) = x < y



-- test trees
singleNode = Node 1349 Nil Nil
dSingleNode = Node 2698 Nil Nil
simpleTree = Node 2 (Node 3 Nil Nil) (Node 2 Nil Nil)
sqSimpleTree = Node 4 (Node 9 Nil Nil) (Node 4 Nil Nil)
stringTree = Node "root" (Node "left" Nil Nil) (Node "right" Nil Nil)

-- to allow free order in the associative array implementation
(=?=) :: (Ord a) => [a] -> [a] -> Bool
l =?= l' = sort l == sort l'
infix 4 =?=

test1a = [
    assert (foldrT (+) 20 Nil == 20) "Success for foldrT (+) 20 Nil",
    assert (foldrT (+) 0 singleNode == 1349) "Success for foldrT (+) 0 singleNode",
    assert (foldrT (+) 20 singleNode == 1369) "Success for foldrT (+) 20 singleNode",
    assert (foldrT (+) 0 simpleTree == 7) "Success for foldrT (+) 0 simpleTree",
    assert (foldrT (^) 1 simpleTree == 512) "Success for foldrT (^) 1 simpleTree",
    assert (foldrT (++) "" stringTree == "rootleftright") "Success for foldrT (++) \"\" stringTree"
    ]

test1b = [
    assert (mapT (==3) Nil == Nil) "Success for mapT (*2) Nil",
    assert (mapT (*2) singleNode == dSingleNode) "Success for mapT (*2) singleNode",
    assert (mapT (length) stringTree == Node 4 (Node 4 Nil Nil) (Node 5 Nil Nil)) "Success for mapT (length) stringTree"
    ]

test1c = [
   assert (zipWithT (^^) Nil Nil == Nil) "Success for zipWithT (^^) Nil Nil",
   assert (zipWithT (const) singleNode Nil == Nil) "Success for zipWithT (const) singleNode Nil",
   assert (zipWithT (div) Nil singleNode == Nil) "Success for zipWithT (div) Nil singleNode",
   assert (zipWithT (+) singleNode singleNode == dSingleNode) "Success for zipWithT (+) singleNode singleNode",
   assert (zipWithT (*) simpleTree simpleTree == sqSimpleTree) "Success for zipWithT (*) simpleTree simpleTree",
   assert (zipWithT (*) singleNode simpleTree == dSingleNode) "Success for zipWithT (*) singleNode simpleTree"
   ]

test2a = [
    assert (insert 1 2 [] == [(1, 2)]) "Success for insert 1 2 []",
    assert (insert 1 "two" [] == [(1, "two")]) "Success for insert 1 \"two\" []",
    assert (insert "one" 2 [] == [("one", 2)]) "Success for insert \"one\" 2 []",
    assert (insert 1 2 [(3, 4), (5, 6)] =?= [(1, 2), (3, 4), (5, 6)]) "Success for insert 1 2 [(3, 4), (5, 6)]",
    assert (insert 1 2 [(1, 4), (5, 6)] =?= [(1, 2), (5, 6)]) "Success for insert 1 2 [(1, 4), (5, 6)]"
    ]

test2b = [
    assert (getValue 1 ([] :: Assoc Int Int) == Nothing) "Success for getValue 1 []",
    assert (getValue 1 [(2, 4), (3, 9)] == Nothing) "Success for getValue 1 [(2, 4), (3, 9)]",
    assert (getValue 1 [(1, 1), (2, 4), (3, 9)] == Just 1) "Success for getValue 1 [(1, 1), (2, 4), (3, 9)]"
    ]

test2c = [
    assert (removeKey 1 ([] :: Assoc Int Int) == []) "Success for removeKey 1 []",
    assert (removeKey 1 [(2, 4), (3, 9)] =?= [(2, 4), (3, 9)]) "Success for removeKey 1 [(2, 4), (3, 9)]",
    assert (removeKey 1 [(1, 1), (2, 4), (3, 9)] =?= [(2, 4), (3, 9)]) "Success for removeKey 1 [(1, 1), (2, 4), (3, 9)]"
    ]

test2d = [
    assert (getKeys ([] :: Assoc Int Int) == []) "Success for getKeys []",
    assert (getKeys [(1, 1)] == [1]) "Success for getKeys [(1, 1)]",
    assert (getKeys [(1, 1), (2, 4), (3, 9)] =?= [1, 2, 3]) "Success for getKeys [(1, 1), (2, 4), (3, 9)]"
    ]


test3a = [
    assert (extSum (Value 3) (Value 2) == Value 5) "Success for extSum (Value 3) (Value 2)",
    assert (extSum (Value 3) Infinity == Infinity) "Success for extSum (Value 3) Infinity",
    assert (extSum Infinity (Value 2) == Infinity) "Success for extSum Infinity (Value 2)"
    ]

test3b = [
    assert (extDiv (Value 8) (Value 2) == Value 4) "Success for extDiv (Value 8) (Value 2)",
    assert (extDiv (Value 8) (Value 3) == Value 2) "Success for extDiv (Value 8) (Value 2)",
    assert (extDiv (Value 8) (Value 0) == Infinity) "Success for extDiv (Value 8) (Value 0)",
    assert (extDiv (Value 8) Infinity == Value 0) "Success for extDiv (Value 8) Infinity",
    assert (extDiv Infinity (Value 3) == Infinity) "Success for extDiv Infinity (Value 3)"
    ]

test3c = [
    assert (extLess (Value 3) (Value 9) == True) "Success for extLess (Value 3) (Value 9)",
    assert (extLess (Value 9) (Value 3) == False) "Success for extLess (Value 9) (Value 3)",
    assert (extLess (Value 101) Infinity == True) "Success for extLess (Value 101) Infinity",
    assert (extLess Infinity (Value 2999) == False) "Success for extLess Infinity (Value 2999)",
    assert (extLess Infinity Infinity == False) "Success for extLess Infinity Infinity"
    ]

allTests = [test1a, 
	    test1b, test1c, test2a, test2b, test2c, test2d,
            test3a, test3b, test3c]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test
