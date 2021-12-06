module Problems.Day06 (solution) where

import Data.MultiSet (MultiSet, size, fromList, concatMap)
import Data.List.Split (splitOn)

import Common.Solution (Day)

simulate :: MultiSet Integer -> MultiSet Integer
simulate = Data.MultiSet.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])

readInput :: String -> MultiSet Integer
readInput = fromList . map read . splitOn ","

solution :: Day
solution = (
        show . size . (!! 80) . iterate simulate . readInput,
        show . size . (!! 256) . iterate simulate . readInput
    )
