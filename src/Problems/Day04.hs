module Problems.Day04 (solution) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (inits, transpose)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))

import Common.Solution (Day)

zeroHits :: [Integer] -> [[Integer]] -> [[Integer]]
zeroHits n b = [[if elem c n then 0 else c | c <- r] | r <- b]

bingo :: [Integer] -> [[Integer]] -> Bool
bingo n b = any ((== 0) . sum) $ let q = zeroHits n b in (q ++ transpose q)

solveA :: [Integer] -> [[[Integer]]] -> Integer
solveA n b = head . catMaybes $ [go n' b | n' <- inits n]
    where
        go ns bs = listToMaybe . catMaybes $ [go' ns b' | b' <- bs]
        go' ns b'
            | bingo ns b' = Just $ (last ns) * (sum . map sum $ zeroHits ns b')
            | otherwise = Nothing

solveB :: [Integer] -> [[[Integer]]] -> Integer
solveB n b = solveA n . head . filter ((== 1) . length) $
             [[i | i <- b, not . bingo n' $ i] | n' <- inits n]

readInput :: String -> ([Integer], [[[Integer]]])
readInput = (rL &&& rR) . splitOn "\n\n"
    where
        rL = map (read :: String -> Integer) . splitOn "," . head
        rR = map (filter (not . null) . map (map (read :: String -> Integer) .
             filter (not . null) . splitOn " ") . splitOn "\n") . tail

solution :: Day
solution = (
        show . uncurry solveA . readInput,
        show . uncurry solveB . readInput
    )
