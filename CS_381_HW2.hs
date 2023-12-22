--- Benjamin Anderson II 934-353-159
--- Due Date: Apr. 25, 2023

--- This code is for HW2 from CS_381 @ Oregon State Univeristy Sp. 2023

module HW2 where
import HW2types
import Data.List

--- Problem 1: Trees ---

singleton :: Int -> Tree
singleton x = Node x Leaf Leaf

treeInsert :: Int -> Tree -> Tree
treeInsert x Leaf = singleton x
treeInsert x (Node a l r)
	| x == a = Node a l r
	| x < a  = Node a (treeInsert x l) r
	| x > a  = Node a l (treeInsert x r)

buildTree :: [Int] -> Tree
buildTree xs = foldr treeInsert Leaf xs

inorder :: Tree -> [Int]
inorder Leaf = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r


-- Part A: sizeTree --
sizeTree :: Tree -> Int
sizeTree t = length (inorder t)


-- Part B: height --
height :: Tree -> Int
height Leaf = -1
height (Node a l r)
	| height l < height r = height r + 1
	| otherwise = height l + 1


-- Part C: treeSum --
treeSum :: Tree -> Int
treeSum t = foldr (+) 0 (inorder t)


-- Part D: Operator Overload --
intSort :: [Int] -> [Int]
intSort[]=[]
intSort (x:xs) = intSort ys ++ [x] ++ intSort zs
	where
		ys = [ a | a <- xs, a <= x]
		zs = [ b | b <- xs, b > x ]

instance Eq Tree where
    (==) j k = 
        case (j, k) of 
            ((Node a b c), (Node x y z)) -> intSort (inorder (Node a b c)) == intSort (inorder (Node x y z))
            ((Node a b c), (Leaf)) -> False
            ((Leaf), (Node a b c)) -> False
            (Leaf, Leaf) -> True


-- Part E: mergeTrees --
mergeTrees :: Tree -> Tree -> Tree
mergeTrees t1 t2 = foldr treeInsert t1 (inorder t2)


-- Part F: isBST --
isBST :: Tree -> Bool
isBST t = inorder t == intSort(inorder t)


--Part G: convertBST--
convertBST :: Tree -> Tree
convertBST t = buildTree (inorder t)



--- GRAPHS ---

-- Part A: numVE --
tuple2List :: Graph -> [Int]
tuple2List [] = []
tuple2List ((a,b):xs) = a : b : tuple2List xs

numVE :: Graph -> (Int, Int)
numVE [] = (0,0)
numVE g = (length (nub (tuple2List g)), length g)


--Part B: removeLoops --
removeLoops :: Graph -> Graph
removeLoops g = [ (x,y) | (x,y) <- g, x /= y]


-- Part C: removeVertex --
removeVertex :: Vertex -> Graph -> Graph
removeVertex a g = [ (x,y) | (x,y) <- g, x /= a, y /= a]
