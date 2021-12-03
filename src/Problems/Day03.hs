module Problems.Day03 (solution) where

import Data.List (transpose)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)

import Common.Solution (Day)

toDecimal :: [Bool] -> Integer
toDecimal [] = 0
toDecimal (x:xs) = (if x then 1 else 0) + 2 * toDecimal xs

partB :: (Integer -> Integer -> Bool) -> [[Bool]] -> [Bool]
partB f xs
    | (length xs) == 1 = head xs
    | any null $ xs = []
    | otherwise = [discr] ++ (partB f . map tail . filter ((== discr) . head) $ xs)
    where
        discr = uncurry f . both ((toInteger) . length) . ((filter id) &&& (filter not)) . map head $ xs

boolAdd :: (Integer, Integer) -> Bool -> (Integer, Integer)
boolAdd (x, y) True = (x + 1, y)
boolAdd (x, y) False = (x, y + 1)

solution :: Day
solution = (
        o . ((map (uncurry (<=))) &&& (map (uncurry (>)))) . map (foldl boolAdd (0, 0)) . transpose . r,
        o . ((partB (>=)) &&& (partB (<))) . r
    )
    where
        r = map (map (== '1')) . lines
        o = show . uncurry (*) . both (toDecimal . reverse)
