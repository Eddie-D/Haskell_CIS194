toDigits :: Integer -> [Integer]
toDigits n
 | n < 10 = [n]
 | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

rev:: [Integer] -> [Integer]
rev [] = []
rev (x:arr) = rev arr ++ [x]

revToDigits :: Integer -> [Integer]
revToDigits x = rev (toDigits x)

-- Doubles left to right, starting with the second digit
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (y:x:xs) = y : 2*x : doubleEveryOtherLeft(xs) 
doubleEveryOtherLeft x = x

-- Doubles right to left
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = doubleEveryOtherLeft(rev(xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
 | x < 10 = x + sumDigits(xs)
 | otherwise = sumDigits(toDigits(x)) + sumDigits(xs)

validate :: Integer -> Bool
validate x = (sumDigits(doubleEveryOther(toDigits x))) `mod` 10 == 0

main :: IO ()
main = print (validate 4012888888881881)