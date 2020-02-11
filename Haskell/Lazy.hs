{-|
 - Paradigme de Programare CB
 - Laborator 8
 -}
module Lazy where

import Control.Exception (assert)

-- I. 1
--naturals = undefined

naturals = 0:(map (+1) naturals)

-- I. 2
--evens = undefined

evens = 0:(map (+2) evens)

-- I. 3
fibonacci :: Num a => [a]
--fibonacci = undefined

fibonacci = 1:1:(zipWith (+) fibonacci (tail fibonacci))

-- II
-- Puteți să vă folosiți de următoarea constantă în restul funcțiilor și să o
-- modificați într-un singur loc (aici) dacă doriți o precizie mai bună
tolerance = 0.001

-- II. 1
-- comportamentul ar trebui să fie identic cu cel al funcției "iterate", care
-- deja există în Prelude
build :: (a -> a) -> a -> [a]
--build = undefined

build f a0 = a0:(map f (build f a0))

-- II. 2
select :: (Num a, Ord a) => a -> [a] -> a
--select = undefined

select e [] = 0
select e (x:x':xs)
	| abs (x - x') < e = x
	| otherwise = select e xs
 

-- II. 3
phi :: Double
--phi = undefined

phi = select tolerance (zipWith (/) (tail fibonacci) fibonacci)

-- II. 4
pi' :: Double
--pi' = undefined

sir = build (\x -> x + sin x) 1
pi' = select tolerance sir

-- II. 5
babylonianSqrt :: Double -> Double
--babylonianSqrt = undefined

babylonianSqrt k = select tolerance (build sir2 1) 
	where sir2 an = 0.5 * (an + (k / an)) 

-- II. 6
newtonRaphson :: (Double -> Double) -> (Double -> Double) -> Double
--newtonRaphson = undefined

newtonRaphson f f' = select tolerance (build sir3 1)
	where sir3 an = an - (f an) / (f' an)

-- II. 7. a
halves :: Double -> [Double]
--halves = undefined

halves a0 = build (\an -> an / 2) a0 

-- II. 7. b
diffs :: (Double -> Double) -> Double -> [Double]
--diffs = undefined

diffs f a = map sir4 (halves 1)
	where sir4 h = (f(a + h) - f(a)) / h

-- II. 7. c
diff :: (Double -> Double) -> Double -> Double
--diff = undefined

diff f a = select tolerance (diffs f a)

-- II. 8. a
areaT :: (Double -> Double) -> Double -> Double -> Double
--areaT = undefined

areaT f a b = 0.5 * (b - a) * (f a + f b)

-- II. 8. b
mid :: Double -> Double -> Double
mid a b = a + (b - a) / 2

addMid :: [Double] -> [Double]
--addMid = undefined

addMid [] = []
addMid [x, y] = [x, mid x y, y]
addMid (x:xs@(y:_)) = x:(mid x y):(addMid xs)


-- II. 8. c
areas :: (Double -> Double) -> [Double] -> [Double]
--areas = undefined

areas f [] = [] 
areas f [x, y] = [areaT f x y]
areas f (x:xs@(y:_)) = (areaT f x y):(areas f xs)

-- II. 8. d
integrate :: (Double -> Double) -> Double -> Double -> Double
--integrate = undefined

integrate f a b = select tolerance (map sumAreas (build addMid [a, b]))
	where sumAreas = sum . (areas f)

-- TEST AREA --
class Approx a where
    approx :: Double -> a -> a -> Bool

instance Approx Double where
    approx e a b = abs (a - b) < e

instance (Approx a) => Approx [a] where
    approx _ [] [] = True
    approx e (x:xs) (y:ys) = approx e x y && approx e xs ys


(≈) :: (Approx a) => a -> a -> Bool
(≈) = approx (2 * tolerance)
infix 4 ≈

-- Approximate set membership
e ∈ s = any (e ≈) s
infix 4 ∈

-- Approximate multiple of
multipleOf m d = qr ≈ qi
    where qr = m / d
          qi = fromIntegral $ round $ m / d

testI1 = [
    assert (not . null $ naturals) "Success!1aaaa",
    assert (head naturals == 0) "Success!",
    assert (naturals !! 1 == 1) "Success!",
    assert (naturals !! 7 == 7) "Success!",
    assert (naturals !! 200 == 200) "Success!"
    ]

testI2 = [
    assert (not . null $ evens) "Success!1bbbb",
    assert (head evens == 0) "Success!",
    assert (evens !! 1 == 2) "Success!",
    assert (evens !! 7 == 14) "Success!",
    assert (evens !! 200 == 400) "Success!"
    ]

testI3 = [
    assert (not . null $ fibonacci) "Success!1ccccc",
    assert (head fibonacci == 1) "Success!",
    assert (fibonacci !! 1 == 1) "Success!",
    assert (fibonacci !! 7 == 21) "Success!",
    assert (fibonacci !! 20 == 10946) "Success!"
    ]

testII3 = [
    assert (phi ≈ 1.61803398) "Success!2aaa"
    ]

testII4 = [
    -- pi already exists in Prelude
    assert (pi' ≈ pi) "Success!2bbb"
    ]

testII5 = [
    assert (babylonianSqrt 0 ≈ 0) "Success!3aaaa",
    assert (babylonianSqrt 1 ≈ 1) "Success!",
    assert (babylonianSqrt 4 ≈ 2) "Success!",
    assert (babylonianSqrt 9 ≈ 3) "Success!",
    assert (babylonianSqrt 2 ≈ sqrt 2) "Success!",
    assert (babylonianSqrt 3 ≈ sqrt 3) "Success!",
    assert (babylonianSqrt pi ≈ sqrt pi) "Success!"
    ]

testII6 = [
    assert (newtonRaphson id (const 1) ≈ 0) "Success!3bbbb",
    assert (newtonRaphson (2 *) (const 2) ≈ 0) "Success!",
    assert (newtonRaphson (+ 1) (const 1) ≈ -1) "Success!",
    assert (newtonRaphson ((-) 1) (const 1) ≈ 1) "Success!",
    assert (newtonRaphson (\x -> 2 * x + 1) (const 2) ≈ -0.5) "Success!",
    assert (newtonRaphson (** 2) (* 2) ≈ 0) "Success!",
    assert (newtonRaphson (\x -> x ** 2 - 1) (2 *) ∈ [-1, 1]) "Success!",
    assert (newtonRaphson (\x -> x ** 2 - 4) (2 *) ∈ [-2, 2]) "Success!",
    assert (newtonRaphson (\x -> x ** 2 - 2) (2 *) ∈ [- (sqrt 2), sqrt 2]) "Success!",
    assert (newtonRaphson sin cos `multipleOf` pi) "Success!",
    assert ((newtonRaphson cos (negate . sin) + pi/2) `multipleOf` pi) "Success!"
    ]

testII7a = [
    assert (and $ zipWith (≈) (halves 0) [0, 0, 0, 0]) "Success!3ccc",
    assert (halves 0 !! 100 == 0) "Success!",
    assert (and $ zipWith (≈) (halves 8) [8, 4, 2, 1]) "Success!",
    assert (and $ zipWith (≈) (halves 1) [1, 0.5, 0.25, 0.125]) "Success!"
    ]

testII7c = [
    assert (diff id 1 ≈ 1) "Success!3dddd",
    assert (diff id 2 ≈ 1) "Success!",
    assert (diff (** 2) 1 ≈ 2) "Success!",
    assert (diff (** 2) 2 ≈ 4) "Success!",
    assert (diff (\x -> x ** 2 + 1) 2 ≈ 4) "Success!",
    assert (diff (\x -> x ** 2 + 1337) 2 ≈ 4) "Success!",
    assert (diff sin 0 ≈ 1) "Success!",
    assert (diff sin pi ≈ -1) "Success!",
    assert (diff sin (pi/2) ≈ 0) "Success!",
    assert (diff sin (pi/4) ≈ (sqrt 2) / 2) "Success!",
    assert (diff sin (pi/4) ≈ sin (pi/4)) "Success!",
    assert (diff cos 0 ≈ 0) "Success!",
    assert (diff cos pi ≈ 0) "Success!",
    assert (diff cos (pi/2) ≈ -1) "Success!",
    assert (diff cos (pi/4) ≈ -(cos (pi/4))) "Success!"
    ]

testII8a = [
    assert (areaT id 0 1 ≈ 0.5) "Success!4aaaa",
    assert (areaT id 0 10 ≈ 50) "Success!",
    assert (areaT id 1 10 ≈ 49.5) "Success!",
    assert (areaT id 3 8 ≈ 27.5) "Success!",
    assert (areaT id 1.5 2 ≈ 0.875) "Success!",
    assert (areaT (+1) 0 1 ≈ 1.5) "Success!",
    assert (areaT (*2) 0 1 ≈ 1.0) "Success!",
    assert (areaT (** 2) 0 1 ≈ 0.5) "Success!",
    assert (areaT (** 2) 2 6 ≈ 80) "Success!",
    assert (areaT sqrt 2 6 ≈ 7.72740) "Success!",
    assert (areaT exp 1 3 ≈ 22.80381) "Success!",
    assert (areaT sin 10 20 ≈ 1.844620) "Success!",
    assert (areaT cos 10 20 ≈ -2.15494733) "Success!"
    ]

testII8b = [
    assert (addMid [1, 3] ≈ [1, 2, 3]) "Success!4bbbb",
    assert (addMid [1, 2] ≈ [1, 1.5, 2]) "Success!",
    assert (addMid [1, 10] ≈ [1, 5.5, 10]) "Success!",
    assert (addMid [1, 3, 5] ≈ [1, 2, 3, 4, 5]) "Success!",
    assert (take 10 (addMid evens) ≈ take 10 naturals) "Success!"
    ]

testII8c = [
    assert (areas id [1, 2] ≈ [1.5]) "Success!4cccc",
    assert (areas id [1, 8] ≈ [31.5]) "Success!",
    assert (areas id [0, 7] ≈ [24.5]) "Success!",
    assert (areas (+ 1) [0, 7] ≈ [31.5]) "Success!",
    assert (areas (* 2) [0, 7] ≈ [49]) "Success!",
    assert (areas (** 2) [0, 7] ≈ [171.5]) "Success!",
    assert (areas sin [0, 7, 14] ≈ [2.29945, 5.766657]) "Success!",
    assert (areas sin [1..5] ≈ [0.87538, 0.525208, -0.30784, -0.8578]) "Success!",
    assert (areas cos [1..5] ≈ [0.06207, -0.7030, -0.821818, -0.18499]) "Success!"
    ]

testII8d = [
    assert (integrate id 0 1 ≈ 0.5) "Success!4dddd",
    assert (integrate id 1 1 ≈ 0) "Success!",
    assert (integrate id 5 5 ≈ 0) "Success!",
    assert (integrate id 1 2 ≈ 1.5) "Success!",
    assert (integrate id 1 10 ≈ 49.5) "Success!",
    assert (integrate (+ 1) 1 10 ≈ 58.5) "Success!",
    assert (integrate (* 2) 1 10 ≈ 99) "Success!",
    assert (integrate (** 2) 1 10 ≈ 333) "Success!",
    assert (integrate (2 **) 1 10 ≈ 1474.4354) "Success!",
    assert (integrate sin 1 4 ≈ 1.19307) "Success!",
    assert (integrate cos 1 4 ≈ -1.5971) "Success!",
    assert (integrate atan 1 4 ≈ 3.4465) "Success!",
    assert (integrate sqrt 3 7 ≈ 8.88221) "Success!"
    ]

allTests = [testI1, testI2, testI3, testII3, testII4, testII5, testII6,
            testII7a, testII7c, testII8a, testII8b, testII8c, testII8d]

runAll = mapM_ runTest allTests
runTest test = mapM_ putStrLn test
