module Problems.Day14 (solution) where

import Control.Arrow ((&&&))
import Data.MultiSet (MultiSet, fromList, toOccurList, concatMap)
import Data.Map (Map, empty, insert, lookup)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (both)

import Common.Solution (Day)

type Pair = (Char, Char)
type Rules = (Map Pair Char)

solve :: Int -> Rules -> MultiSet Pair -> Integer
solve n m
    = uncurry (-)
    . both (`div` 2)
    . (maximum &&& minimum)
    . fmap (toInteger . snd)
    . filter ((/= '_') . fst)
    . toOccurList
    . Data.MultiSet.concatMap (\(c1,c2) -> [c1, c2])
    . (!! n)
    . iterate (Data.MultiSet.concatMap ccmf)
    where
        ccmf p@(c1, c2) = case Data.Map.lookup p m of
            (Just cn) -> [(c1, cn), (cn, c2)]
            (Nothing) -> [p]

readInput :: String -> (Rules, MultiSet Pair)
readInput s = (mp, fromList (zip ("_" ++ th) (th ++ "_")))
    where
        (th:bh:[]) = splitOn "\n\n" s
        ff m ((k1:k2:[]):(v:[]):[]) = insert (k1, k2) v m
        ff _ _ = error "Bad pattern match"
        mp = foldl ff empty . fmap (splitOn " -> ") . lines $ bh

solution :: Day
solution = (
        show . uncurry (solve 10) . readInput,
        show . uncurry (solve 40) . readInput
    )
