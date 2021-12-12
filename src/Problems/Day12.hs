module Problems.Day12 (solution) where

import Data.MultiSet (singleton, notMember, insert, toOccurList)
import Data.Tuple (swap)
import Data.Char (isUpper)
import Data.Map (Map, empty, insertWith, findWithDefault)
import Data.List.Split (splitOn)

import Common.Solution (Day)

data Node
    = Small String
    | Big String
    | Start
    | End
    deriving (Show, Eq)

instance Ord Node where
    compare n1 n2 = compare (show n1) (show n2)

type Graph = Map Node [Node]

readNode :: String -> Node
readNode "start" = Start
readNode "end" = End
readNode s
    | isUpper . head $ s = Big s
    | otherwise = Small s

readPath :: String -> (Node, Node)
readPath s = (readNode lhs, readNode rhs)
    where
        (lhs:rhs:[]) = splitOn "-" s

readGraph :: String -> Graph
readGraph s = foldr rf empty (paths ++ (map swap paths))
    where
        paths = map readPath . lines $ s
        rf (f, t) = insertWith (++) f [t]

allPaths :: Bool -> Graph -> [[Node]]
allPaths r gr = go gr Start (singleton Start)
    where
        go g n s
            | n == End = [[End]]
            | otherwise = [
                    n:p |
                    l <- findWithDefault [] n g,
                    case l of
                        (Start) -> False
                        (Small _) -> ((notMember l ns) || (r && allowReuse))
                        _ -> True,
                    p <- (go g l ns)
                ]
            where
                ns = case n of {(Small _) -> insert n s; _ -> s}
                allowReuse = and . map ((<= 1) . snd) . toOccurList $ ns

solution :: Day
solution = (
        show . length . (allPaths False) . readGraph,
        show . length . (allPaths True) . readGraph
    )
