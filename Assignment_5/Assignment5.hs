import ExprT
import Parser (parseExp)
import StackVM (Program)

-- Part 1 -----------------------------
eval :: ExprT -> Integer
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Lit x) = x

-- Part 2 -----------------------------
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Just exp -> Just (eval exp)
    Nothing -> Nothing

-- Part 3 -----------------------------
class Expr t where
    add :: t -> t -> t 
    mul :: t -> t -> t 
    lit :: Integer -> t

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Part 4 -----------------------------
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = lit (max x y)
    mul (MinMax x) (MinMax y) = lit (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 . mod 7
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)
