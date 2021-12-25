module Problems.Day20 (solution) where

import Data.List.Split (splitOn)
import Data.Map (fromList, keys, findWithDefault, filter, size)

import Common.Geometry (Point2D, Grid2D, readGrid2D)
import Common.Solution (Day)

neighbours :: Point2D -> [Point2D]
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

readInput :: String -> ([Bool], (Grid2D Bool), Bool)
readInput s = (fmap (\x -> x == '#') th, im, False)
    where
        (th:lh:[]) = splitOn "\n\n" s
        im = fmap (== '#') . readGrid2D $ lh

step :: [Bool] -> (Grid2D Bool, Bool) -> (Grid2D Bool, Bool)
step l (m, d) = (fromList np, nd)
    where
        nd = (l !!) . toDecimal . replicate 9 $ d
        np =
            [ (c, v)
            | p <- keys m
            , c <- neighbours p
            , let ns = neighbours c
            , let v = (l !!) . toDecimal . fmap (\x -> findWithDefault d x m) $ ns
            ]

solve :: Int -> ([Bool], (Grid2D Bool), Bool) -> Int
solve n (l, m, d) = size . Data.Map.filter id $ fm
    where
        (fm, _) = (!! n) . iterate (step l) $ (m, d)

solution :: Day
solution = (
        show . (solve 2) . readInput,
        show . (solve 50) . readInput
    )
