-- Exercise 1 ------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -------------------------
fibn :: Integer -> Integer -> [Integer]
fibn x y = x + y : fibn y (x + y)

fibs2 :: [Integer]
fibs2 = fibn 0 1

-- Exercise 3 -------------------------
data Stream a = Stream a (Stream a)

-- Show first 10 elements
instance Show a => Show (Stream a) where
    show = show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

-- Exercise 4 -------------------------
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- Exercise 5 --------------------------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

greatestTwo :: Integer -> Integer
greatestTwo n
    | even n = 1 + greatestTwo (n `div` 2)
    | otherwise = 0

ruler :: Stream Integer
ruler = streamMap greatestTwo (streamFromSeed (+1) 1)

-- Exercise 6 --------------------------
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger y = Stream y (streamRepeat 0)
    negate = streamMap negate 
    (+) (Stream x xs) (Stream y ys) = Stream (x + y) (xs + ys) 
    (*) (Stream x xs) fy@(Stream y ys) = Stream (x*y) ((fromInteger x *ys) + (xs*fy))

instance Fractional (Stream Integer) where
    (/) a@(Stream x xs) b@(Stream y ys) = Stream (x `div` y) ((xs - (a/b) * ys)/fromInteger y)


fibs3 :: [Integer]
fibs3 = streamToList (x/(1-x-x*x))