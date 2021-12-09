module Problems.Day09 (solution) where

import Data.List (sort, nub)
import Data.Set (Set, size, singleton, filter, map, toList, fromList, empty, difference, union)
import Data.Map (Map, fromList, filterWithKey, foldr, findWithDefault, elems, mapWithKey)

import Common.Solution (Day)

type Coordinate = (Integer, Integer)

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

setNeighbours :: Set Coordinate -> Set Coordinate
setNeighbours s = flip difference s . Data.Set.fromList . concatMap neighbours . toList $ s

readInput :: String -> Map Coordinate Integer
readInput = Data.Map.fromList . zip2d . lines
    where
        zip2d l = concat [[((x, y), read [c]) | (x, c) <- zip [0..] r] | (y, r) <- zip [0..] l]

solveA :: Map Coordinate Integer -> Integer
solveA m = Data.Map.foldr ((+) . (+ 1)) 0 . filterWithKey f $ m
    where
        f c v = and . fmap (\x -> findWithDefault 10 x m > v) . neighbours $ c

solveB :: Map Coordinate Integer -> Integer
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
