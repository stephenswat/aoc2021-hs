module Problems.Day08 (solution) where

import Data.Tuple (swap)
import Data.Maybe (fromJust, catMaybes)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, size, filter, findMin, isSubsetOf)
import Data.Map (Map, empty, insert, lookup, toList, fromList)
import Data.Bifunctor (first)

import Common.Solution (Day)

type Predicate = Map Integer (Set Char) -> Set Char -> Bool

(<&&>) :: Predicate -> Predicate -> Predicate
(<&&>) f g m s = (f m s) && (g m s)

solve :: Set (Set Char) -> Map Integer (Set Char)
solve i
    = solver 0 (filterNeq 6 <&&> filterNeq 9 <&&> filterSize 6)
    . solver 6 (filterNeq 9 <&&> filterSuperset 5 <&&> filterSize 6)
    . solver 2 (filterNeq 5 <&&> filterNeq 3 <&&> filterSize 5)
    . solver 5 (filterNeq 3 <&&> filterSize 5 <&&> filterSubset 9)
    . solver 9 (filterSuperset 3 <&&> filterSize 6)
    . solver 3 (filterSuperset 7 <&&> filterSize 5)
    . solver 8 (filterSize 7)
    . solver 7 (filterSize 3)
    . solver 4 (filterSize 4)
    . solver 1 (filterSize 2)
    $ empty
    where
        solver k f m = insert k (findMin . Data.Set.filter (f m) $ i) m
        filterNeq v m = (/= (fromJust . Data.Map.lookup v $ m))
        filterSize v _ = (== v) . size
        filterSubset v m x = x `isSubsetOf` (fromJust . Data.Map.lookup v $ m)
        filterSuperset v m = isSubsetOf (fromJust . Data.Map.lookup v $ m)

toNumber :: [Integer] -> Integer
toNumber [] = 0
toNumber ns = last ns + 10 * (toNumber . init $ ns)

solution :: Day
solution = (
        run (length . Prelude.filter (\x -> x == 1 || x == 7 || x == 4 || x == 8) . catMaybes . concat),
        run (sum . map (toNumber . catMaybes))
    )
    where
        readLeft lhs = Data.Set.fromList . map Data.Set.fromList . splitOn " " $ lhs
        readRight rhs = map Data.Set.fromList . splitOn " " $ rhs
        readLine l = let (lhs:rhs:[]) = splitOn " | " l in (readLeft lhs, readRight rhs)
        swapMap = Data.Map.fromList . map swap . Data.Map.toList
        render (m, s) = [Data.Map.lookup q m | q <- s]
        run f = show . f . map (render . (first (swapMap . solve)) . readLine) . lines
