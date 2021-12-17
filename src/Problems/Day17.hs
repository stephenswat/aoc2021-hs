module Problems.Day17 (solution) where

import Text.Regex.PCRE ((=~))
import Data.List (nub)
import Data.Bifunctor (bimap)

import Common.Solution (Day)

type Pair = (Integer, Integer)

stepX :: Pair -> Pair
stepX (p, v)
    | v == 0    = (p, v)
    | v > 0     = (p + v, v - 1)
    | otherwise = (p + v, v + 1)

stepY :: Pair -> Pair
stepY (p, v) = (p + v, v - 1)

solve :: (Pair -> Pair) -> Pair -> [Pair]
solve f (lo, hi) =
    [ (i, x)
    | x <- [-1000..1000]
    , (i, (p, _)) <- take 1000 . zip [0..] . iterate f $ (0, x)
    , p >= lo && p <= hi
    ]

readInput :: String -> (Pair, Pair)
readInput s = ((read lx, read hx), (read ly, read hy))
    where
        pat = "target area: x=(\\-?\\d+)..(\\-?\\d+), y=(\\-?\\d+)..(\\-?\\d+)"
        (_:lx:hx:ly:hy:[]) = head (s =~ pat)

solution :: Day
solution = (
        show . (\(_, (ly, _)) -> (-ly * (-ly + 1)) `div` 2 + ly) . readInput,
        show . length . nub . uncurry solveB . solve2 . readInput
    )
    where
        solveB xs ys = [(x, y) | (xi, x) <- xs, (yi, y) <- ys, xi == yi]
        solve2 = bimap (solve stepX) (solve stepY)
