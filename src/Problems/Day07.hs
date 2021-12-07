module Problems.Day07 (solution) where

import Data.List.Split (splitOn)

import Common.Solution (Day, notImplemented)

solve :: (Integer -> Integer) -> [Integer] -> Integer
solve f x = minimum $ [sum [f (j - i) | j <- x] | i <- [(minimum x)..(maximum x)]]

solution :: Day
solution = (s abs, s ((\i -> (i * (i + 1)) `div` 2) . abs))
    where s f = show . solve f . map read . splitOn ","
