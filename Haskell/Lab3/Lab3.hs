{-|
 - Paradigme de Programare CB
 - Laborator 3
 -}
module Lab3 where

import Control.Exception (assert)

import Helper

-- 1.a
--line i mat = undefined

line i mat = head (drop i mat)

-- 1.b
--getElem i j mat = undefined

getElem i j mat = head (drop j (line i mat)) 

-- 1.c
--matSum mat1 mat2 = undefined

matSum = zipWith(zipWith(+))

-- 1.d
--transpose mat = undefined

transpose ([]:_) = []
transpose mat = (map head mat) : transpose(map tail mat)

-- 1.e
--matProd mat1 mat2 = undefined

matProd mat1 mat2 = map(\line -> map(\col -> foldr (+) 0 (zipWith (*) line col)) (transpose mat2) ) mat1

-- 2.a
--hflip img = undefined

hflip = reverse

-- 2.b
--vflip img = undefined

vflip = map reverse 

-- 2.c
--rotate90trig img = undefined

rotate90trig = hflip.transpose

-- 2.d
--rotate90clockwise img = undefined

rotate90clockwise = vflip.transpose

-- 2.e
--negative img = undefined

negative = (map.map) (\x -> if x == ' ' then '*' else ' ')

-- 2.f
--scale x img = undefined

scale x = replEach x.map (replEach x)
		where replEach x = foldr (\e acc -> (replicate x e) ++ acc) []

-- 2.g
--catH img = undefined

catH = zipWith (++)

-- 2.h
--catV img = undefined

catV = (++)

-- 2.i
--cropV x y mat = undefined

cropV x y = drop x.(take (y+1))

-- 2.j
--cropH x y mat = undefined

cropH x y = map (cropV x y)


test1a = [
    assert (line 0 [[1], [2]] == [1]) "Success for line 0 [[1], [2]]",
    assert (line 1 [[1], [2]] == [2]) "Success for line 1 [[1], [2]]",
    assert (line 2 m2 == [1, 0, 1]) "Success for line 2 m2",
    assert (line 15 img == "***   *  *          ***   *  *          ") "Success for line 15 img"
    ]

test1b = [
    assert (line 0 [[1], [2]] == [1]) "Success for line 0 [[1], [2]]",
    assert (line 1 [[1], [2]] == [2]) "Success for line 1 [[1], [2]]",
    assert (line 2 m2 == [1, 0, 1]) "Success for line 2 m2",
    assert (line 15 img == "***   *  *          ***   *  *          ") "Success for line 15 img"
    ]

test1c = [
    assert (matSum [[1]] [[2]] == [[3]]) "Success for matSum [[1]] [[2]]",
    assert (matSum [[1, 2]] [[3, 4]] == [[4, 6]]) "Success for matSum [[1, 2]] [[3, 4]]",
    assert (matSum [[1], [2]] [[3], [4]] == [[4], [6]]) "Success for matSum [[1], [2]] [[3], [4]]",
    assert (matSum m1 m2 == summ1m2) "Success for matSum m1 m2"
    ]

test1d = [
    assert (transpose [[1]] == [[1]]) "Success for transpose [[1]]",
    assert (transpose [[1], [2]] == [[1, 2]]) "Success for transpose [[1], [2]]",
    assert (transpose [[1, 2]] == [[1], [2]]) "Success for transpose [[1, 2]]",
    assert (transpose m1 == [[1,4,7],[2,5,8],[3,6,9]]) "Success for transpose m1"
    ]

test1e = [
    assert (matProd [[2]] [[3]] == [[6]]) "Success for matProd [[2]] [[3]]",
    assert (matProd [[2, 3]] [[2], [3]] == [[13]]) "Success for matProd [[2, 3]] [[2], [3]]",
    assert (matProd m1 [[1, 0, 0], [0, 1, 0], [0, 0, 1]] == m1) "Success for matProd m1 [[1, 0, 0], [0, 1, 0], [0, 0, 1]]",
    assert (matProd m1 m2 == prodm1m2) "Success for matProd m1 m2"
    ]

test2a = [
    assert (hflip [l1] == [l1]) "Success for hflip l1",
    assert (hflip [l1, l2, l3] == [l3, l2, l1]) "Success for hflip [l1, l2, l3]"
    ]

test2b = [
    assert (vflip ["*"] == ["*"]) "Success for vflip [\"*\"]",
    assert (vflip ["* "] == [" *"]) "Success for vflip [\"* \"]",
    assert (vflip ["*", " "] == ["*", " "]) "Success for vflip [\"*\", \" \"]",
    assert (vflip ["*  ", " **", " * "] == ["  *", "** ", " * "]) "Success for vflip [\"*  \", \" **\", \" * \"]"
    ]

test2c = [
    assert (rotate90trig ["*"] == ["*"]) "Success for rotate90trig [\"*\"]",
    assert (rotate90trig ["* "] == [" ", "*"]) "Success for rotate90trig [\"* \"]",
    assert (rotate90trig ["*", " "] == ["* "]) "Success for rotate90trig [\"*\", \" \"]",
    assert (rotate90trig ["* ", "* "] == ["  ", "**"]) "Success for rotate90trig [\"* \", \" *\"]"
    ]

test2d = [
    assert (rotate90clockwise ["*"] == ["*"]) "Success for rotate90clockwise [\"*\"]",
    assert (rotate90clockwise ["* "] == ["*", " "]) "Success for rotate90clockwise [\"* \"]",
    assert (rotate90clockwise ["*", " "] == [" *"]) "Success for rotate90clockwise [\"*\", \" \"]",
    assert (rotate90clockwise ["* ", "* "] == ["**", "  "]) "Success for rotate90clockwise [\"* \", \" *\"]"
    ]

test2e = [
    assert (negative [" "] == ["*"]) "Success for negative [\" \"]",
    assert (negative ["*"] == [" "]) "Success for negative [\"*\"]",
    assert (negative ["* "] == [" *"]) "Success for negative [\"* \"]",
    assert (negative ["* *", " **"] == [" * ", "*  "]) "Success for negative [\"* *\", \" **\"]"
    ]

test2f = [
    assert (scale 1 img == img) "Success for scale 1 img",
    assert (scale 2 ["*"] == ["**", "**"]) "Success for scale 2 [\"*\"]",
    assert (scale 3 ["*"] == ["***", "***", "***"]) "Success for scale 3 [\"*\"]",
    assert (scale 2 ["* ", " *"] == ["**  ", "**  ", "  **", "  **"]) "Success for scale 3 [\"* \", \" *\"]"
    ]

test2g = [
    assert (catH ["*"] ["*"] == ["**"]) "Success for catH [\"*\"] [\"*\"]",
    assert (catH ["* *"] ["***"] == ["* ****"]) "Success for catH [\"* *\"] [\"***\"]",
    assert (catH ["* *", "** "] ["   ", "***"] == ["* *   ", "** ***"]) "Success for catH [\"* *\", \"** \"] [\"   \", \"***\"]"
    ]

test2h = [
    assert (catV ["*"] ["*"] == ["*", "*"]) "Success for catV [\"*\"] [\"*\"]",
    assert (catV ["* *"] ["***"] == ["* *", "***"]) "Success for catV [\"* *\"] [\"***\"]",
    assert (catV ["* *", "** "] ["   ", "***"] == ["* *", "** ", "   ", "***"]) "Success for catV [\"* *\", \"** \"] [\"   \", \"***\"]"
    ]

test2i = [
    assert (cropV 0 0 ["*"] == ["*"]) "Success for cropV 0 0 [\"*\"]",
    assert (cropV 0 0 ["*  *"] == ["*  *"]) "Success for cropV 0 0 [\"*  *\"]",
    assert (cropV 1 1 ["**", "  ", "* "] == ["  "]) "Success for cropV 1 1 [\"**\", \"  \", \"* \"]",
    assert (cropV 1 2 ["**", "  ", "* "] == ["  ", "* "]) "Success for cropV 1 2 [\"**\", \"  \", \"* \"]"
    ]

test2j = [
    assert (cropH 0 0 ["*"] == ["*"]) "Success for cropH 0 0 [\"*\"]",
    assert (cropH 0 0 ["*  *"] == ["*"]) "Success for cropH 0 0 [\"*  *\"]",
    assert (cropH 1 1 ["**", "  ", "* "] == ["*", " ", " "]) "Success for cropH 1 1 [\"**\", \"  \", \"* \"]",
    assert (cropH 1 2 ["** ", "   ", "* *"] == ["* ", "  ", " *"]) "Success for cropH 1 2 [\"** \", \"   \", \"* *\"]"
    ]



allTests = [test1a, test1b, test1c, test1d, test1e,
            test2a, test2b, test2c, test2d, test2e, test2f, test2g, test2h, test2i, test2j]

runAll = mapM_ (mapM_ putStrLn) allTests
runTest test = mapM_ putStrLn test
