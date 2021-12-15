module Problems.Day15 (solution) where

import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map, fromList, singleton, insertWith, lookup, unions, mapKeys)
import Data.Set (Set, singleton, empty, null, delete, notMember, insert, member)

import Common.Solution (Day)

type Coordinate = (Integer, Integer)
type Grid = Map Coordinate Integer

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

scale :: Grid -> Grid
scale g = unions
    [ mapKeys (\(x, y) -> (100 * nx + x, 100 * ny + y))
    . fmap (\x -> ((x - 1 + nx + ny) `mod` 9) + 1)
    $ g
    | nx <- [0..4]
    , ny <- [0..4]
    ]

readGrid :: String -> Grid
readGrid s = fromList
    [ ((x, y), read [c])
    | (y, r) <- zip [0..] (lines s)
    , (x, c) <- zip [0..] r
    ]

dijkstra :: Coordinate -> Coordinate -> Grid -> Grid
dijkstra c dst g = go Data.Set.empty (Data.Set.singleton c) (Data.Map.singleton c 0)
    where
        go :: Set Coordinate -> Set Coordinate -> Grid -> Grid
        go a x d
            | Data.Set.null x = d
            | Data.Set.member dst a = d
            | otherwise = go na nx nd
            where
                u = minimumBy (compare `on` (fromJust . flip (Data.Map.lookup) d)) $ x
                (Just w) = Data.Map.lookup u d
                na = Data.Set.insert u a
                nb = [(p, w + (fromJust nw)) | p <- neighbours u, notMember p na, let nw = Data.Map.lookup p g, isJust nw]
                nx = foldl (\m (n, _) -> Data.Set.insert n m) (delete u x) nb
                nd = foldl (\m (n, wt) -> Data.Map.insertWith min n wt m) d nb

solution :: Day
solution = (
        show . Data.Map.lookup (99, 99) . dijkstra (0, 0) (99, 99) . readGrid,
        show . Data.Map.lookup (499, 499) . dijkstra (0, 0) (499, 499) . scale . readGrid
    )
