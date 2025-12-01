-- Exercise 1 ---------------------------------------
-- Skips setup nskips with an initial value of 1
skips :: [a] -> [[a]]
skips = nskips 1

-- Recursively runs skip on a reducing list
nskips :: Int -> [a] -> [[a]]
nskips i (x:xs) = skip i (x:xs) : nskips (i+1) xs
nskips _ _ = []

-- Recursively reads the nth element starting with the first one
skip :: Int -> [a] -> [a]
skip i (x:xs) = x : skip i (drop (i-1) xs)
skip _ _ = []


-- Exercise 2 ----------------------------------------
localMaxima :: [Integer] -> [Integer]
localMaxima (x:p@(y:z:_)) 
    | x < y && y > z = y : localMaxima p
    | otherwise = localMaxima p
localMaxima _ = []

-- Exercise 3 ----------------------------------------
histogram :: [Integer] -> String
histogram x = unlines (hString (maximum (hList x)) (hList x) ) ++ hFooter

-- Creates the legend
hFooter :: String
hFooter =  ['=' | _<-[1..10]] ++ "\n" ++ ['0'..'9'] ++ "\n"

-- Returns list containing the frequency of each index
hList :: [Integer] -> [Integer]
hList x = [fromIntegral (length (filter (== i) x)) | i <- [0..9]]

-- Recursively produces a string representation for each line
hString :: Integer -> [Integer] -> [String]
hString i xs  
    | i <= 0 = []
    | otherwise = [if j >= i then '*' else ' ' | j <- xs] : hString (i-1) xs
