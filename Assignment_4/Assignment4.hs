-- Exercise 1 ------------------------
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3*n + 1)

-- Exercise 2 --------------------------
foldTree :: [a] -> Tree a
foldTree = foldr putTree Leaf

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving Show

putTree :: a -> Tree a -> Tree a
putTree i Leaf = Node 0 Leaf i Leaf
putTree i (Node _ t1 x t2)
    | tHeight t1 < tHeight t2 = Node (tHeight (putTree i t1) + 1) (putTree i t1) x t2
    | otherwise = Node (tHeight (putTree i t2) + 1) t1 x (putTree i t2)


tHeight :: Tree a -> Integer
tHeight Leaf = -1
tHeight (Node h _ _ _) = h

-- Exercise 3 ----------------------------
xor :: [Bool] -> Bool
-- xor = foldr (\x y -> (x || y) && not (x && y))  False
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a = foldr (flip f) a . reverse

-- Exercise 4 -----------------------------
sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map ((+1) . (*2)) (filter (`notElem` sundaramRemove x) [1..x])

sundaramRemove :: Integer -> [Integer]
sundaramRemove x = [i + j + (2 * i * j) | i <- [1..x], j <- [i..x]]