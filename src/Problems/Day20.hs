module Problems.Day20 (solution) where

import Data.List.Split (splitOn)
import Data.Map (Map, fromList, keys, findWithDefault, filter, size)

import Common.Solution (Day)

type Point = (Integer, Integer)

neighbours :: Point -> [Point]
neighbours (x, y) = [
        (x - 1, y - 1),
        (x    , y - 1),
        (x + 1, y - 1),
        (x - 1, y    ),
        (x    , y    ),
        (x + 1, y    ),
        (x - 1, y + 1),
        (x    , y + 1),
        (x + 1, y + 1)
    ]

toDecimal :: [Bool] -> Int
toDecimal = foldl (\a n -> 2 * a + (if n then 1 else 0)) 0

readInput :: String -> ([Bool], (Map Point Bool), Bool)
readInput s = (map (\x -> x == '#') th, fromList im, False)
    where
        (th:lh:[]) = splitOn "\n\n" s
        im = [((x, y), c == '#') | (y, r) <- zip [0..] (lines lh), (x, c) <- zip [0..] r]

step :: [Bool] -> (Map Point Bool, Bool) -> (Map Point Bool, Bool)
step l (m, d) = (fromList np, nd)
    where
        nd = (l !!) . toDecimal . replicate 9 $ d
        np =
            [ (c, v)
            | p <- keys m
            , c <- neighbours p
            , let ns = neighbours c
            , let v = (l !!) . toDecimal . map (\x -> findWithDefault d x m) $ ns
            ]

solve :: Int -> ([Bool], (Map Point Bool), Bool) -> Int
solve n (l, m, d) = size . Data.Map.filter id $ fm
    where
        (fm, _) = (!! n) . iterate (step l) $ (m, d)

solution :: Day
solution = (
        show . (solve 2) . readInput,
        show . (solve 50) . readInput
    )
