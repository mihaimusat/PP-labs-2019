{-|
 - Paradigme de Programare CB
 - Laborator 7
 -
 - Definirea funcțiilor de substituție și a celor de evaluare.
 -
 - Acesta e fișierul în care ar trebui să lucrați și pe care să-l încărcați în
 - ghci (celălalte fișiere utile sunt importate de aici). Tot aici sunt și
 - testele automate, scrise într-un format (cât de cât) lizibil.
 -}
module Eval where

import Expr
import ParseExpr
import Parser
import Control.Exception (assert)
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as S


-- Întoarce o nouă variabilă care nu mai apare în expresia dată
unusedVar :: Expr -> String
unusedVar e = let vs = vars e
                  available_lc = S.difference lowercase vs
              in if S.null available_lc
                    then foldr (\x y -> show x ++ y) "" vs
                    else head $ S.toList available_lc

lowercase = S.fromList (map (:"") ['a'..'z'])

-- Substituie într-o expresie, o variabilă cu o altă expresie.
--
-- Folosiți informațiile despre regulile de substituție de aici:
-- http://ocw.cs.pub.ro/ppcarte/doku.php?id=pp:lambda#the_semantics_of_the_lambda_calculus
--
-- În situații în care trebuie să redenumiți variabile, aveți la dispoziție
-- funcția "unusedVar" care primește o expresie și vă întoarce o variabilă nouă,
-- nemaifolosită în corpul expresiei
subst :: Expr        -- expresia în care înlocuim
      -> Expr        -- expresia cu care înlocuim
      -> String      -- variabila înlocuită
      -> Expr        -- expresia rezultată
--subst = undefined
subst (Var y) par x 
	| y == x = par
	| otherwise = (Var y)
subst (Abs y body) par x
	| (x /= y) && y `notElem` (freeVars par) = Abs y (subst body par x)
	| (x /= y) = Abs z (subst body' par x)
	| otherwise = Abs y body
	where z = (unusedVar par)
	      body' = subst body (Var z) y 
subst (App e1 e2) par x = App (subst e1 par x) (subst e2 par x)

-- Efectuează un singur pas de evaluare normală.
normal :: Expr -> Expr
--normal = undefined

normal (App e1 e2) = 
	case e1 of 
		(Abs x body) -> subst body e2 x
		_ -> App (normal e1) e2
normal e = e

-- Efectuează un singur pas de evaluare aplicativă.
applicative :: Expr -> Expr
--applicative = undefined

appFree :: Expr -> Bool
appFree (Var _) = True
appFree (Abs _ e) = appFree e
appFree (App _ _) = False

applicative (App e1 e2) = 
	case e1 of
        (Abs x b)
            | appFree e2 -> subst b e2 x
            | otherwise  -> App e1 (applicative e2)
        _ -> App (applicative e1) e2
applicative e = e


-- Evaluator "big-step", care evaluează expresia până la capăt. Puteți opri
-- evaluarea atunci când nu se mai realizează progres, i.e. expresia rezultată e
-- egală cu expresia de la care ați pornit.
eval :: (Expr -> Expr)      -- unul dintre "normal" și "applicative"
     -> Expr                -- expresia de evaluat
     -> Expr                -- expresia rezultat
--eval = undefined

eval fn e 
	| (fn e) == e = e
	| otherwise = eval fn (fn e)
	

-- TEST AREA --
-- unsafely convert string to expression
stoe :: String -> Expr
stoe = fromJust . parse expr

-- work on strings for readability
subst' :: String -> String -> String -> Expr
subst' e e' x = subst (stoe e) (stoe e') x

normal' :: String -> Expr
normal' = normal . stoe

applicative' :: String -> Expr
applicative' = applicative . stoe

eval' :: (Expr -> Expr) -> String -> Expr
eval' stepper = eval stepper . stoe

-- compare structurally, but with string
(=~) :: Expr -> String -> Bool
e1 =~ e2 = e1 == stoe e2
infix 4 =~

-- strict comparison of expressions
(=!) :: Expr -> String -> Bool
e1 =! e2 = show e1 == e2
infix 4 =!

-- (attempt to) compare two expressions semantically
--
-- https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence
-- https://en.wikipedia.org/wiki/Deductive_lambda_calculus#Intensional_versus_extensional_equality
(=?) :: Expr -> String -> Bool
e1 =? e2 = e1 == e2' && freeVars e1 == freeVars e2'
    where e2' = stoe e2
infix 4 =?

test1 = [
    assert ((subst' "x" "y" "z") =! "x") "Success1",
    assert ((subst' "x" "y" "x") =! "y") "Success2",
    assert ((subst' "λx.(x y)" "z" "x") =! "λx.(x y)") "Success3",
    assert ((subst' "λx.(x y)" "z" "y") =! "λx.(x z)") "Success4",
    assert ((subst' "λx.λx.x" "y" "x") =! "λx.λx.x") "Success5",
    assert ((subst' "(x z)" "y" "x") =! "(y z)") "Success6",
    assert ((subst' "(x x)" "y" "x") =! "(y y)") "Success7",
    assert ((subst' "((x x) x)" "y" "x") =! "((y y) y)") "Success8",
    assert ((subst' "λy.((x x) y)" "z" "x") =! "λy.((z z) y)") "Success9",
    assert ((subst' "λy.((x x) y)" "z" "y") =! "λy.((x x) y)") "Success10",

    assert ((subst' "λx.y" "x" "y") =? "λz.x") "Success11",
    assert ((subst' "λx.(x y)" "x" "y") =? "λz.(z x)") "Success12",
    assert ((subst' "λx.(y λx.y)" "x" "y") =? "λz.(x λz.x)") "Success13"
    ]

test2a = [
    assert ((normal' "x") =~ "x") "Success14",
    assert ((normal' "x") =! "x") "Success15",

    assert ((normal' "(λx.x y)") =~ "y") "Success16",
    assert ((normal' "(λx.x y)") =! "y") "Success17",

    assert ((normal' "(λx.z y)") =~ "z") "Success18",
    assert ((normal' "(λx.z y)") =! "z") "Success19",

    assert ((normal' "(λx.λx.x y)") =~ "λx.x") "Success20",

    assert ((normal' "((λx.x λx.z) (λx.x y))") =~ "(λx.z (λx.x y))") "Success21",

    assert ((normal' "(λx.z (λx.x y))") =~ "z") "Success22",
    assert ((normal' "(λx.z (λx.x y))") =! "z") "Success23",

    assert ((normal' "(λx.x (λx.x y))") =~ "(λx.x y)") "Success24",
    assert ((normal' "(λx.x (λx.z y))") =~ "(λx.z y)") "Success25",

    assert ((normal' "(λx.(x x) λx.(x x))") =~ "(λx.(x x) λx.(x x))") "Success26",
    assert ((normal' "(λx.y (λx.(x x) λx.(x x)))") =~ "y") "Success27"
    ]

test2b = [
    assert ((applicative' "x") =~ "x") "Success28",
    assert ((applicative' "x") =! "x") "Success29",

    assert ((applicative' "(λx.x y)") =~ "y") "Success30",
    assert ((applicative' "(λx.x y)") =! "y") "Success31",

    assert ((applicative' "(λx.z y)") =~ "z") "Success32",
    assert ((applicative' "(λx.z y)") =! "z") "Success33",

    assert ((applicative' "(λx.λx.x y)") =~ "λx.x") "Success34",

    assert ((applicative' "((λx.x λx.z) (λx.x y))") =~ "(λx.z (λx.x y))") "Success35",

    assert ((applicative' "(λx.z (λx.x y))") =~ "(λx.z y)") "Success36",

    assert ((applicative' "(λx.x (λx.x y))") =~ "(λx.x y)") "Success37",
    assert ((applicative' "(λx.x (λx.z y))") =~ "(λx.x z)") "Success38",

    assert ((applicative' "(λx.(x x) λx.(x x))") =~ "(λx.(x x) λx.(x x))") "Success39",
    assert ((applicative' "(λx.y (λx.(x x) λx.(x x)))") =~ "(λx.y (λx.(x x) λx.(x x)))") "Success40"
    ]

test3 = [
    assert ((eval' normal "(λa.a ((λa.λb.a c) d))") =! "c") "Success41",
    assert ((eval' applicative "(λa.a ((λa.λb.a c) d))") =! "c") "Success42",

    assert ((eval' normal "(λa.a ((λa.λb.b c) d))") =! "d") "Success43",
    assert ((eval' applicative "(λa.a ((λa.λb.b c) d))") =! "d") "Success44",

    assert ((eval' normal "((λa.a λa.a) ((λa.λb.b c) d))") =! "d") "Success45",
    assert ((eval' applicative "((λa.a λa.a) ((λa.λb.b c) d))") =! "d") "Success46",

    assert ((eval' normal "(λx.x (λy.z (λx.(x x) λx.(x x))))") =! "z") "Success47",
    assert ((eval' applicative "(λx.x (λy.z (λx.(x x) λx.(x x))))") =! "(λx.x (λy.z (λx.(x x) λx.(x x))))") "Success48"
    ]

allTests = [test1, test2a, test2b, test3]

runAll = mapM_ (mapM_ putStrLn) allTests
