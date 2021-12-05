module Problems.Day05 (solution) where

import Data.List.Split (splitOn)
import Data.Tuple.Extra (both)
import Data.MultiSet (MultiSet, fromList, empty, union, distinctSize, filter, occur)

import Common.Solution (Day)

type Point = (Integer, Integer)
type Line = (Point, Point)

isValidA :: Line -> Bool
isValidA ((x1, y1), (x2, y2)) = (x1 == x2) || (y1 == y2)

isValidB :: Line -> Bool
isValidB l@((x1, y1), (x2, y2)) = isValidA l || abs (x2 - x1) == abs (y2 - y1)

points :: Line -> MultiSet Point
points ((x1, y1), (x2, y2))
    | x1 == x2  = fromList [(x1, y) | y <- [(minY) .. (maxY)]]
    | y1 == y2  = fromList [(x, y1) | x <- [(minX) .. (maxX)]]
    | otherwise = fromList [(x1 + d * dX, y1 + d * dY) | d <- [0 .. (maxY - minY)]]
    where
        minX = min x1 x2
        minY = min y1 y2
        maxX = max x1 x2
        maxY = max y1 y2
        dX = if x2 > x1 then 1 else -1
        dY = if y2 > y1 then 1 else -1

readInput :: String -> [Line]
readInput = map (both (tup . map read . splitOn ",") . tup . splitOn " -> ") . lines
    where
        tup (x:y:[]) = (x, y)
        tup _ = error "Bad tuple list"

multiples :: Ord a => MultiSet a -> MultiSet a
multiples s = Data.MultiSet.filter ((> 1) . (flip occur) s) s

solution :: Day
solution = (
        s isValidA,
        s isValidB
    )
    where
        s f = show . distinctSize . multiples . foldl union empty . map points . Prelude.filter f . readInput
