module Problems.Day25 (solution) where

import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Data.Map (filter, keys, findWithDefault, insert)

import Common.Geometry (Grid2D, readGrid2D)
import Common.Solution (Day)

data Tile
    = Horizontal
    | Vertical
    | Empty
    deriving (Eq, Ord, Show)

parseGrid :: String -> Grid2D Tile
parseGrid = fmap parseTile . readGrid2D
    where
        parseTile '.' = Empty
        parseTile '>' = Horizontal
        parseTile 'v' = Vertical
        parseTile _   = error "Not a valid tile"

step :: Grid2D Tile -> Grid2D Tile
step = subStep Vertical . subStep Horizontal
    where
        subStep :: Tile -> Grid2D Tile -> Grid2D Tile
        subStep k g = foldl (\a (s, d) -> insert s Empty . insert d k $ a) g move
            where
                limX = (+ 1) . maximum . map fst . keys $ g
                limY = (+ 1) . maximum . map snd . keys $ g
                dX = if k == Horizontal then 1 else 0
                dY = if k == Vertical   then 1 else 0
                move
                    = Prelude.filter (\(_, d) -> (findWithDefault Empty d g) == Empty)
                    . map (id &&& (\(x, y) -> ((x + dX) `mod` limX, (y + dY) `mod` limY)))
                    . keys
                    . Data.Map.filter (== k)
                    $ g

findStable :: Eq a => (a -> a) -> a -> Maybe (Int, a)
findStable f s = go 0 (iterate f s)
    where
        go :: Eq a => Int -> [a] -> Maybe (Int, a)
        go i (p1:p2:ps)
            | p1 == p2 = Just (i + 1, p1)
            | otherwise = go (i + 1) (p2:ps)
        go _ _ = Nothing

solution :: Day
solution = (
        show . fst . fromJust . findStable step . parseGrid,
        (\_ -> "Merry Christmas!")
    )
