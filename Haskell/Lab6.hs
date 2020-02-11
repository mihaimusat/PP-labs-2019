module Lab6 where

import Data.Maybe

--------------------------------------------------------------------------------
{-
    Clasa PQueue definește interfața pentru toate structurile de coada de
    priorități pe care le vom implementa mai jos.
-}
class PQueue pq where

    -- Construiește o coadă goală de priorități
    empty :: pq a

    -- Verifică dacă e goală
    isEmpty :: pq a -> Bool

    -- Inserează elem în coada de priorități
    insert :: (Ord a) => a -> pq a -> pq a

    -- Întoarce primul element din coada de priorități
    top :: (Ord a) => pq a -> Maybe a

    -- Șterge primul element din coada de priorități
    delete :: (Ord a) => pq a -> pq a

    -- Construiește o coadă de priorități plecând de la o listă de elemente
    fromList :: (Ord a) => [a] -> pq a
    fromList = foldr insert empty

    -- Lista elementelor din coada de priorități
    toList :: (Ord a) => pq a -> [a]
    toList = toListAux []
        where getTop = fromJust . top
              toListAux acc pq
                | isEmpty pq  = acc
                | otherwise = toListAux (getTop pq : acc) (delete pq)


--------------------------------------------------------------------------------

-- 1. a
newtype MinQueue a = MinQueue {getElems::[a]} deriving Show
	
instance PQueue MinQueue where 
		empty = MinQueue []
		
		isEmpty (MinQueue mq) = null mq

		
		insert e (MinQueue []) = MinQueue [e]
		insert e (MinQueue (x:xs)) = MinQueue (insertAux e (x:xs)) where 
						insertAux e [] = [e]						
						insertAux e (x:xs) = if (e < x) then e:x:xs
								     else x:(insertAux e xs)
	
		top (MinQueue []) = Nothing
		top (MinQueue (x:xs)) = Just x
		
		delete (MinQueue []) = MinQueue []
		delete (MinQueue (x:xs)) = MinQueue xs

newtype MaxQueue a = MaxQueue {getValues::[a]} deriving Show

instance PQueue MaxQueue where 

	empty = MaxQueue []
	
	isEmpty (MaxQueue mq) = null mq

	insert e (MaxQueue []) = MaxQueue [e]
	insert e (MaxQueue (x:xs)) = MaxQueue (insertAux e (x:xs)) where
					insertAux e [] = [e]
					insertAux e (x:xs) = if (e > x) then e:x:xs
							     else x:(insertAux e xs)

	top (MaxQueue []) = Nothing
	top (MaxQueue (x:xs)) = Just x

	delete (MaxQueue []) = MaxQueue []
	delete (MaxQueue (x:xs)) = MaxQueue xs


--un leftist heap este frunza sau un nod in care tin :
--rank-ul = distanta de la nod la cel mai din dreapta nod
--nodeVal = un tuplu in care tin (prioritate, element)
--left = subarbore stang
--right = subarbore drept

data LeftistPQ a = Empty { rank :: Int } |
                   Node { rank :: Int, nodeVal :: (Int, a), left :: LeftistPQ a, right :: LeftistPQ a }

merge :: LeftistPQ a -> LeftistPQ a -> LeftistPQ a
merge (Empty _) node = node
merge node (Empty _) = node
merge a@(Node _ elem1 left1 right1) b@(Node _ elem2 left2 right2)
	| (fst elem1 > fst elem2) = makeNode elem1 left1 (merge right1 b) 
	| otherwise = makeNode elem2 left2 (merge a right2)
	where makeNode elem left right = if (rank left < rank right) 
						then (Node (1+rank left) elem right left)
						else (Node (1+rank right) elem left right)

inorder :: LeftistPQ a -> [(Int, a)]
inorder (Empty _) = []
inorder pq = (inorder $ left pq) ++ [nodeVal pq] ++ (inorder $ right pq)

--definire structuri pentru teste
valsInt = [20, 100, 30, 500, 1000, 30023, 513]
listPQInt :: MinQueue Int
listPQInt = fromList valsInt

valsInt2 = [40, 200, 50, 600, 5000, 70023, 413]
listPQInt2 :: MaxQueue Int
listPQInt2 = fromList valsInt2

emptyNode = Empty 0
node1 = Node 1 (3, 4) emptyNode emptyNode
node2 = Node 1 (5, 10) emptyNode emptyNode
node3 = Node 1 (10, 20) emptyNode emptyNode
node4 = Node 1 (4, 10) emptyNode emptyNode

l1 = inorder $ merge emptyNode node1
l2 = inorder $ merge node1 emptyNode
l3 = inorder $ merge node1 node2
l4 = inorder $ merge node4 $ merge node3 $ merge node1 node2

