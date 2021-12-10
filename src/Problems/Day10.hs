module Problems.Day10 (solution) where

import Data.List (sort)
import Data.Either (lefts, rights)

import Common.Solution (Day)

median :: Ord a => [a] -> a
median l
    | length l `mod` 2 == 1 = (sort l) !! (length l `div` 2)
    | otherwise = error "Median list must be odd length!"

scoreA :: [Char] -> Integer
scoreA = sum . map val
    where
        val ')' = 3
        val ']' = 57
        val '}' = 1197
        val '>' = 25137
        val _   = error "Not a valid character"

scoreB :: [String] -> Integer
scoreB = median . map (foldl (\x y -> x * 5 + val y) 0)
    where
        val '(' = 1
        val '[' = 2
        val '{' = 3
        val '<' = 4
        val _   = error "Not a valid character"

check :: String -> String -> Either Char String
check [] s = Right s
check ('(':r) s = check r ('(':s)
check ('{':r) s = check r ('{':s)
check ('[':r) s = check r ('[':s)
check ('<':r) s = check r ('<':s)
check (')':r) (t:s) = if t == '(' then check r s else Left ')'
check ('}':r) (t:s) = if t == '{' then check r s else Left '}'
check (']':r) (t:s) = if t == '[' then check r s else Left ']'
check ('>':r) (t:s) = if t == '<' then check r s else Left '>'
check _ _ = error "Invalid character in string!"

solution :: Day
solution = (
        show . scoreA . lefts . map (\x -> check x []) . lines,
        show . scoreB . rights . map (\x -> check x []) . lines
    )
