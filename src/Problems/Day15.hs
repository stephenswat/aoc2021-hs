module Problems.Day15 (solution) where

import Data.Maybe (isJust, fromJust)
import Data.Map (lookup, unions, mapKeys)

import Common.Algorithm (search)
import Common.Geometry (Point2D, Grid2D, neighbours4, readGrid2D)
import Common.Solution (Day)

scale :: Grid2D Integer -> Grid2D Integer
scale g = unions
    [ mapKeys (\(x, y) -> (100 * nx + x, 100 * ny + y))
    . fmap (\x -> ((x - 1 + nx + ny) `mod` 9) + 1)
    $ g
    | nx <- [0..4]
    , ny <- [0..4]
    ]

run :: Point2D -> Grid2D Integer -> Maybe (Point2D, Integer)
run d g = search (== d) (\x -> [(p, fromJust w) | p <- neighbours4 x, let w = Data.Map.lookup p g, isJust w]) (0, 0)

solution :: Day
solution = (
        show . run (99, 99) . fmap (\x -> read [x]) . readGrid2D,
        show . run (499, 499) . scale . fmap (\x -> read [x]) . readGrid2D
    )
