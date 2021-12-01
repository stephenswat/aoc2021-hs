module Problems.Day01 (solution) where

import Common.Solution (Day)

pairs :: [a] -> [(a, a)]
pairs (a:b:xs) = [(a, b)] ++ pairs (b:xs)
pairs _ = []

triples :: [a] -> [(a, a, a)]
triples (a:b:c:xs) = [(a, b, c)] ++ triples(b:c:xs)
triples _ = []

solution :: Day
solution = (
        o . r,
        o . map (\(a, b, c) -> a + b + c) . triples . r
    )
    where
        r :: String -> [Integer]
        r = map (read :: (String -> Integer)) . lines

        o :: [Integer] -> String
        o = show . length . filter (< 0) . map (uncurry (-)) . pairs
