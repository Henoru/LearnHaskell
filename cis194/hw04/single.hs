
module Single where
import Data.List

-- ex01
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x-2) *fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x ->x-2) .filter even

fun2 :: Integer -> Integer
fun2 1=0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise =  fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even .takeWhile (/= 1) . iterate (\x->if even x then x `div` 2 else 3*x+1)

-- ex02
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ lt root rt)
      | getH newRt > getH lt +1 =rotL (Node (uniH lt newRt) lt root newRt)
      | otherwise = Node (uniH lt newRt) lt root newRt
        where
          newRt = insert x rt
    rotL Leaf = Leaf
    rotL (Node hf lt x rt@(Node hr rlt y rrt))=Node (uniH newLt rrt) newLt y rrt
      where
        newLt =Node (uniH lt rlt) lt x rlt
    rotL (Node _ _ _ Leaf)=error "Can't left rotate"
    getH Leaf = -1
    getH (Node h _ _ _)=h
    uniH lt rt = max (getH lt) (getH rt) + 1

-- ex03
xor :: [Bool] -> Bool
xor = odd . length . filter id

map' :: (a->b)->[a]->[b]
map' f =foldr (\x y->f x:y) [] 

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse 

-- ex04

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =map (\x->2*x+1) ([1..n] \\ [x+y+2*x*y|x<-[1..n],y<-[1..n]])

