type Peg = String
type Move = (Peg, Peg)

-- p1: tower, p2: goal, p3: storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ (p1, p2) : hanoi (n-1) p3 p2 p1

main :: IO ()
main = print (hanoi 4 "a" "b" "c")