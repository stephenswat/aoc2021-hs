module Problems.Day02 (solution) where

import Data.List.Split (splitOn)

import Common.Solution (Day)

data Command
    = Forward Integer
    | Up Integer
    | Down Integer

parse :: String -> Command
parse s = (f c) . (read :: (String -> Integer)) $ n
    where
        (c:n:_) = splitOn " " s

        f :: String -> Integer -> Command
        f "forward" = Forward
        f "up" = Up
        f "down" = Down
        f _ = error "Invalid instruction"

translateA :: (Integer, Integer) -> Command -> (Integer, Integer)
translateA (h, d) (Forward n) = (h + n, d)
translateA (h, d) (Up n) = (h, d - n)
translateA (h, d) (Down n) = (h, d + n)

translateB :: (Integer, Integer, Integer) -> Command -> (Integer, Integer, Integer)
translateB (h, d, a) (Up n) = (h, d, a - n)
translateB (h, d, a) (Down n) = (h, d, a + n)
translateB (h, d, a) (Forward n) = (h + n, d + a * n, a)

solution :: Day
solution = (
        show . uncurry (*) . foldl translateA (0, 0) . map parse . lines,
        show . (\(h, d, _) -> h * d) . foldl translateB (0, 0, 0) . map parse . lines
    )
