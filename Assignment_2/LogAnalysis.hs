{-# OPTIONS_GHC -Wall #-}
import Log
import Text.Read (readMaybe)

-- Util -----------------------------------------
getLog :: Int -> IO [LogMessage]
getLog n = take n . parse <$> readFile "error.log"

-- Exercise 1 -----------------------------------
parseMessage :: String -> LogMessage
parseMessage m = case parseType (words m) of
    Just lm -> lm
    Nothing -> Unknown m

parseType :: [String] -> Maybe LogMessage
parseType ("E":s:rest) = case readMaybe s of
    Just x -> parseTime (Error x) rest
    _ -> Nothing
parseType ("I":rest) = parseTime Info rest
parseType ("W":rest) = parseTime Warning rest
parseType _ = Nothing

parseTime :: MessageType -> [String] -> Maybe LogMessage
parseTime messageType (t:rest) = case readMaybe t :: Maybe Int of
    Just i -> Just (LogMessage messageType i (unwords rest))
    _ -> Nothing
parseTime _ _ = Nothing

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2 -----------------------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert l Leaf = Node Leaf l Leaf
insert l@(LogMessage _ time _) (Node left tl@(LogMessage _ treeTime _) right)
 | time > treeTime = Node left tl (insert l right) 
 | time < treeTime = Node (insert l left) tl right
insert _ tree = tree

-- Exercise 3 -----------------------------------
build :: [LogMessage] -> MessageTree
build (m:ms) = insert m (build ms)
build [] = Leaf

-- Exercise 4 -----------------------------------
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left l right) = (inOrder left) ++ l : (inOrder right)
inOrder Leaf = []

-- Exercise 5 -----------------------------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map stringMessage (inOrder (build (filterError lms)))

filterError :: [LogMessage] -> [LogMessage]
filterError (x@(LogMessage (Error _) _ _):xs) = x : (filterError xs)
filterError (_:xs) = filterError xs
filterError [] = []

stringMessage :: LogMessage -> String
stringMessage (LogMessage _ _ m) = m
stringMessage (Unknown m) = m


main :: IO ()
main = do
    logs <- getLog 3815
    print (whatWentWrong logs)