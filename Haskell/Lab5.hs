{-|
 - Paradigme de Programare CB
 - Laborator 5
 -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lab5 where

import Control.Exception (assert)
import Data.List (sort)

-- 1. List
data List a = Empty | Cons a (List a)

instance (Show a) => Show (List a) where
	show l = "[" ++ showAux l ++ "]" where
		showAux Empty = ""
		showAux (Cons x Empty) = show x
		showAux (Cons x xs) = show x ++ "," ++ showAux xs



-- 2. Binary Trees
data BTree a = Nil | Node a (BTree a) (BTree a) deriving Show

-- 2.a
instance (Eq a) => Eq (BTree a) where
	Nil == Nil = True
	(Node k l r) == (Node k' l' r') = (k == k') && (l == l') && (r == r')
	_ == _ = False
	
-- 2.b
instance Foldable BTree where
	foldr f acc Nil = acc
	foldr f acc (Node k l r) = f k (foldr f (foldr f acc r) l)

-- 2.c
instance Functor BTree where
	fmap _ Nil = Nil
	fmap f (Node k l r) = Node (f k) (fmap f l) (fmap f r)
	


-- 3. Extended natural numbers
data Extended = Infinity | Value Integer

-- 3.a
instance Show Extended where
	show Infinity = "Inf"
	show (Value x) = show x

-- 3.b
instance Eq Extended where
	Infinity == Infinity = True
	(Value x) == (Value y) = (x == y)
	_ == _ = False

-- 3.c
instance Ord Extended where
	Infinity `compare` Infinity = EQ
	(Value x) `compare` (Value y) = x `compare` y
	Infinity `compare` _ = GT
	_ `compare` Infinity = LT

-- 3.d
-- puteți folosi "undefined" acolo unde rezultatul nu are sens
instance Num Extended where
	(+) (Value a) (Value b) = Value (a+b)
	(+) Infinity Infinity = Infinity
	(+) Infinity _ = Infinity
	(+) _ Infinity = Infinity
	
	(*) (Value a) (Value b) = Value (a*b)
	(*) Infinity Infinity = Infinity
	(*) Infinity _ = Infinity
	(*) _ Infinity = Infinity
	
	(-) (Value a) (Value b) = Value (a-b)
	(-) Infinity (Value a) = Infinity
	(-) Infinity Infinity = undefined
	
	abs Infinity = Infinity 
	abs (Value x) = Value (abs x)
	
	signum Infinity = 1
	signum (Value x) = Value (signum x)

	fromInteger x = Value x


-- 4. Expr
data Expr a = Val a | Var String | Plus (Expr a) (Expr a) | Mult (Expr a) (Expr a)

-- 5. Eval
type Dictionary a = [(String, a)]

getValue :: String -> Dictionary a -> Maybe a
getValue k [] = Nothing
getValue k ((hk, hv):t)
    | k == hk   = Just hv
    | otherwise = getValue k t

class Eval t a where
    eval :: Dictionary a -> t a -> Maybe a

-- 5.a
instance Eval Expr Integer where
-- eval :: Dictionary Integer -> Expr Integer -> Maybe Integer
	eval d (Val x) = Just x
	eval d (Var s) = getValue s d
	eval d (Plus e1 e2) = case (eval d e1, eval d e2) of
				(Just v1, Just v2) -> Just (v1 + v2)
			        _ -> Nothing
	eval d (Mult e1 e2) = case (eval d e1, eval d e2) of 
				(Just v1, Just v2) -> Just (v1 * v2)
				_ -> Nothing

-- 5.b
instance Eval Expr Extended where
-- eval :: Dictionary Extended -> Expr Extended -> Maybe Extended
	eval d (Val x) = Just x
	eval d (Var s) = getValue s d
	eval d (Plus e1 e2) = case (eval d e1, eval d e2) of
				(Just v1, Just v2) -> Just (v1 + v2)
			        _ -> Nothing
	eval d (Mult e1 e2) = case (eval d e1, eval d e2) of 
				(Just v1, Just v2) -> Just (v1 * v2)
				_ -> Nothing


-- 5.c
{- 

--Datorita faptului ca Integer si Extended sunt parte din Num, 
inseamna ca pot sa sa tin doar Expr a, cu constrangerea sa faca
parte din Num a 

instance (Num a) => Eval Expr a where
	eval d (Val x) = Just x
	eval d (Var s) = getValue s d
	eval d (Sum e1 e2) = case (eval d e1, eval d e2) of
				(Just v1, Just v2) -> Just (v1 + v2)
        			_ -> Nothing
     	eval d (Prod e1 e2) = case (eval d e1, eval d e2) of
         			(Just v1, Just v2) -> Just (v1 * v2)
         			_ -> Nothing
-}

-- test lists
simpleList = Cons 1 (Cons 2 (Cons 3 Empty))
-- test trees
nilTree = Nil :: BTree ()
singleNode = Node 1349 Nil Nil
dSingleNode = Node 2698 Nil Nil
simpleTree = Node 2 (Node 3 Nil Nil) (Node 2 Nil Nil)
simpleTree8 = Node 8 (Node 8 Nil Nil) (Node 8 Nil Nil)


test1 = [
    assert (show (Empty :: List ()) == "[]") "Success for show Empty",
    assert (show (Cons 1 Empty) == "[1]") "Success for show (Cons 1 Empty)",
    assert (show (Cons "string" Empty) == "[\"string\"]")  "Success for show (Cons \"string\" Empty",
    assert (show simpleList == "[1,2,3]") "Success for show simpleList"
    ]

test2a = [
    assert (nilTree == nilTree) "Success for Nil == Nil",
    assert (singleNode == singleNode) "Success for singleNode == singleNode",
    assert (singleNode /= Nil) "Success for singleNode /= Nil",
    assert (Nil /= singleNode) "Success for Nil /= singleNode",
    assert (singleNode /= dSingleNode) "Success for singleNode /= dSingleNode",
    assert (simpleTree == simpleTree) "Success for simpleTree == simpleTree",
    assert (Nil `elem` [singleNode, simpleTree, Nil]) "Success for Nil `elem` [singleNode, simpleTree, Nil]"
    ]

test2b = [
    assert (foldr (^) 1 simpleTree == 512) "Success for foldr (^) 1 simpleTree",
    assert (foldl (^) 1 simpleTree == 1) "Success for foldl (^) 1 simpleTree",
    assert (maximum simpleTree == 3) "Success for maximum simpleTree",
    assert (minimum simpleTree == 2) "Success for minimum simpleTree",
    assert (sum simpleTree == 7) "Success for sum simpleTree",
    assert (product simpleTree == 12) "Success for product simpleTree",
    assert (null Nil) "Success for null Nil",
    assert (not $ null singleNode) "Success for null singleNode",
    assert (length simpleTree == 3) "Success for length simpleTree",
    assert (3 `elem` simpleTree) "Success for 3 `elem` simpleTree",
    assert (not $ 5 `elem` simpleTree) "Success for 5 `elem` simpleTree"
    ]

test2c = [
    assert (fmap id nilTree == nilTree) "Success for fmap id Nil",
    assert (fmap id simpleTree == simpleTree) "Success for fmap id simpleTree",
    assert (fmap (length . show) nilTree == (fmap length . fmap show) nilTree) "Success for fmap (length . show) nilTree",
    assert (fmap (length . show) simpleTree == (fmap length . fmap show) simpleTree) "Success for fmap (length . show) simpleTree",
    assert ((8 <$ nilTree) == Nil) "Success for 8 <$ nilTree",
    assert ((8 <$ simpleTree) == simpleTree8) "Success for 8 <$ simpleTree"
    ]


test3a = [
    assert (show Infinity == "Inf") "Success for show Infinity",
    assert (show (Value 1990) == "1990") "Success for show (Value 1990)"
    ]

test3b = [
    assert (Value 34 == Value 34) "Success for Value 34 == Value 34",
    assert (Value 0 == Value 0) "Success for Value 0 == Value 0",
    assert (Value 1 /= Value 2) "Success for Value 1 /= Value 2",
    assert (Value 8 /= Infinity) "Success for Value 8 /= Infinity",
    assert (Infinity /= Value 8) "Success for Infinity /= Value 8"
    ]

test3c = [
    assert (Value 0 <= Value 1) "Success for Value 0 <= Value 1",
    assert (Value 0 <= Value 0) "Success for Value 0 <= Value 0",
    assert (Value 9001 <= Infinity) "Success for Value 9001 <= Infinity",
    assert (Value 0 < Value 1) "Success for Value 0 < Value 1",
    assert (Value 9001 < Infinity) "Success for Value 9001 < Infinity",
    assert (Value 1 >= Value 0) "Success for Value 1 >= Value 0",
    assert (Value 0 >= Value 0) "Success for Value 0 >= Value 0",
    assert (Infinity >= Value 9001) "Success for Infinity >= Value 9001",
    assert (Value 1 > Value 0) "Success for Value 1 >= Value 0",
    assert (Infinity > Value 9001) "Success for Infinity >= Value 9001",
    assert (not $ Value 0 < Value 0) "Success for Value 0 < Value 0",
    assert (not $ Value 0 > Value 0) "Success for Value 0 > Value 0",
    assert (max (Value 8) (Value 12) == Value 12) "Success for max (Value 8) (Value 12)",
    assert (max (Value 12) (Value 8) == Value 12) "Success for max (Value 12) (Value 8)",
    assert (max (Value 12) Infinity == Infinity) "Success for min (Value 12) Infinity",
    assert (max Infinity (Value 12) == Infinity) "Success for min Infinity (Value 12)",
    assert (min (Value 8) (Value 12) == Value 8) "Success for min (Value 8) (Value 12)",
    assert (min (Value 12) (Value 8) == Value 8) "Success for min (Value 12) (Value 8)",
    assert (min (Value 12) Infinity == Value 12) "Success for min (Value 12) Infinity",
    assert (min Infinity (Value 12) == Value 12) "Success for min Infinity (Value 12)"
    ]

test3d = [
    assert (Value 3 + Value 3 == Value 6) "Success for Value 3 + Value 3",
    assert (Value 3 + Infinity == Infinity) "Success for Value 3 + Infinity",
    assert (Infinity + Value 3 == Infinity) "Success for Infinity + Value 3",
    assert (Infinity + Infinity == Infinity) "Success for Infinity + Infinity",

    assert (Value 3 * Value 3 == Value 9) "Success for Value 3 + Value 3",
    assert (Value 3 * Infinity == Infinity) "Success for Value 3 + Infinity",
    assert (Infinity * Value 3 == Infinity) "Success for Infinity + Value 3",
    assert (Infinity * Infinity == Infinity) "Success for Infinity + Infinity",

    assert (Value 8 - Value 5 == Value 3) "Success for Value 8 - Value 5",
    assert (Value 8 - Value 8 == Value 0) "Success for Value 8 - Value 8",
    assert (Infinity - Value 3000 == Infinity) "Success for Infinity - Value 3000",

    assert ((fromInteger 4 :: Extended) == Value 4) "Success for fromInteger 4",
    assert (3 + Value 3 == Value 6) "Success for 3 + Value 3",
    assert (Infinity + 3 == Infinity) "Success for Infinity + 3",
    assert (3 * Value 3 == Value 9) "Success for 3 + Value 3",
    assert (Infinity * 3 == Infinity) "Success for Infinity + 3"
    ]

allTests = [test1, test2a, test2b, test2c, test3a, test3b, test3c, test3d]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test
