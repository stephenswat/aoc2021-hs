{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module Problems.Day13 (solution) where

import Data.Biapplicative (biliftA2)
import Data.List (stripPrefix, intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, filter, size, map, foldr, member)
import Data.Bifunctor (first, second)

import Common.Solution (Day)

type Coordinate = (Integer, Integer)
data Fold = FoldX Integer | FoldY Integer deriving (Show)

showPaper :: Set Coordinate -> String
showPaper s = intercalate "\n" [[toChar (x, y) | x <- [0..mx]] | y <- [0..my]]
    where
        (mx, my) = Data.Set.foldr (biliftA2 max max) (0, 0) s
        toChar c = if member c s then '#' else '.'

foldPaper :: Fold -> Set Coordinate -> Set Coordinate
foldPaper f
    | (FoldX v) <- f = Data.Set.filter ((< v) . fst) . Data.Set.map (first (q v))
    | (FoldY v) <- f = Data.Set.filter ((< v) . snd) . Data.Set.map (second (q v))
    where
        q v i = if i > v then (2 * v - i) else i

readInput :: String -> (Set Coordinate, [Fold])
readInput s = (cs, fs)
    where
        (th:lh:[]) = splitOn "\n\n" s

        cs = fromList . fmap (\x -> read ("(" ++ x ++ ")")) . lines $ th
        fs = fmap readFold . lines $ lh

        readFold (stripPrefix "fold along x=" -> Just xs) = FoldX . read $ xs
        readFold (stripPrefix "fold along y=" -> Just xs) = FoldY . read $ xs
        readFold _ = error "Not a valid fold"

solution :: Day
solution = (
        show . size . solve . second (take 1) . readInput,
        ("\n" ++) . showPaper . solve . readInput
    )
    where solve = uncurry (foldl (flip foldPaper))
