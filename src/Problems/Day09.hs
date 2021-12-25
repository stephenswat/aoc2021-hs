module Problems.Day09 (solution) where

import Data.List (sort, nub)
import Data.Set (Set, size, singleton, filter, toList, fromList, empty, difference, union)
import Data.Map (Map, filterWithKey, foldr, findWithDefault, elems, mapWithKey, map)

import Common.Solution (Day)
import Common.Geometry (Point2D, neighbours4, readGrid2D)

setNeighbours :: Set Point2D -> Set Point2D
setNeighbours s = flip difference s . Data.Set.fromList . concatMap neighbours4 . toList $ s

readInput :: String -> Map Point2D Integer
readInput = Data.Map.map (\x -> read [x]) . readGrid2D

solveA :: Map Point2D Integer -> Integer
solveA m = Data.Map.foldr ((+) . (+ 1)) 0 . filterWithKey f $ m
    where
        f c v = and . fmap (\x -> findWithDefault 10 x m > v) . neighbours4 $ c

solveB :: Map Point2D Integer -> Integer
solveB m = product . take 3 . reverse . sort . fmap (fromIntegral . size) . nub . elems . mapWithKey findBasin $ m
    where
        findBasinHelper s
            | s == n    = s
            | otherwise = findBasinHelper n
            where
                n = union s . Data.Set.filter (\x -> findWithDefault 10 x m < 9) . setNeighbours $ s

        findBasin c v
            | v < 9 = findBasinHelper . singleton $ c
            | otherwise = empty


solution :: Day
solution = (
        show . solveA . readInput,
        show . solveB . readInput
    )
