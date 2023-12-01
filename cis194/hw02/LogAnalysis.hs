-- Not fully tested
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str=
  let tokens = words str
  in
    case tokens of
        "E":level:time:rs -> LogMessage (Error (read level)) (read time) (unwords rs)
        "I":time:rs -> LogMessage Info (read time) (unwords rs)
        "W":time:rs -> LogMessage Warning (read time) (unwords rs)
        _ -> Unknown str

parse :: String -> [LogMessage]
parse str=[parseMessage x| x <- lines str]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree=tree
insert message Leaf=Node Leaf message Leaf
insert message@(LogMessage _ mtime _) tree@(Node lson root@(LogMessage _ rtime _) rson)
  |mtime < rtime = Node (insert message lson) root rson
  |otherwise = Node lson root (insert message rson)
    
build:: [LogMessage] -> MessageTree
build = foldl insert' Leaf 
  where insert' x y=insert y x

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf=[]
inOrder (Node lson root rson)=inOrder lson ++ [root]++inOrder rson

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages=[logMessage2Str m|m <- inOrder (build (unorderWentWrong logMessages))]
  where
    unorderWentWrong :: [LogMessage] -> [LogMessage]
    unorderWentWrong []=[]
    unorderWentWrong (m@(LogMessage (Error level) _ _):xs)
        | level>=50 = m: unorderWentWrong xs 
        | otherwise = unorderWentWrong xs
    unorderWentWrong (_:xs)=unorderWentWrong xs
    logMessage2Str m=
        case m of
            Unknown x -> x
            LogMessage _ _ str-> str