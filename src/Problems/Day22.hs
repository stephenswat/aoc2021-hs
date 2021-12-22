module Problems.Day22 (solution) where

import Control.Applicative (Alternative (many, (<|>)))
import Data.List (intercalate, nub)

import Common.Solution (Day)
import Common.Parse (Parser, parse, readExact_, readInteger)

type Coordinate = (Integer, Integer, Integer)

data BVH
    = Union BVH BVH
    | DisjointUnion [BVH]
    | Intersection BVH BVH
    | Difference BVH BVH
    | JointDifference BVH BVH
    | Region Coordinate Coordinate
    | Empty
    deriving (Eq)

instance Show BVH where
    show = showAt 0

showAt :: Int -> BVH -> String
showAt d (Empty) = (replicate d ' ') ++ "Empty"
showAt d (Region l h) = (replicate d ' ') ++ "Region " ++ (show l) ++ " to " ++ (show h)
showAt d (Union l r) = (replicate d ' ') ++ "Union (\n" ++ (showAt (d + 2) l) ++ ",\n" ++ (showAt (d + 2) r) ++ "\n" ++ (replicate d ' ') ++ ")"
showAt d (Difference l r) = (replicate d ' ') ++ "Difference (\n" ++ (showAt (d + 2) l) ++ ",\n" ++ (showAt (d + 2) r) ++ "\n" ++ (replicate d ' ') ++ ")"
showAt d (Intersection l r) = (replicate d ' ') ++ "Intersection (\n" ++ (showAt (d + 2) l) ++ ",\n" ++ (showAt (d + 2) r) ++ "\n" ++ (replicate d ' ') ++ ")"
showAt d (DisjointUnion ts) = (replicate d ' ') ++ "DisjointUnion (\n" ++ (intercalate ",\n" [showAt (d + 2) l | l <- ts]) ++ "\n" ++ (replicate d ' ') ++ ")"
showAt d (JointDifference l r) = (replicate d ' ') ++ "JointDifference (\n" ++ (showAt (d + 2) l) ++ ",\n" ++ (showAt (d + 2) r) ++ "\n" ++ (replicate d ' ') ++ ")"

parseInput :: Parser Char [(BVH, Bool)]
parseInput = do many (do r <- parseRegion; readExact_ "\n"; return r)
    where
        parseBoolean = (readExact_ "on" >> return True) <|>
                       (readExact_ "off" >> return False)
        parseRegion = do
            b <- parseBoolean
            readExact_ " x="
            xL <- readInteger
            readExact_ ".."
            xH <- readInteger
            readExact_ ",y="
            yL <- readInteger
            readExact_ ".."
            yH <- readInteger
            readExact_ ",z="
            zL <- readInteger
            readExact_ ".."
            zH <- readInteger
            return ((Region (xL, yL, zL) (xH, yH, zH)), b)

constructTree :: [(BVH, Bool)] -> BVH
constructTree = foldl (\a (t, b) -> if b then Union a t else Difference a t) Empty

simplify :: BVH -> BVH
simplify Empty = Empty
simplify n@(Region (lLx, lLy, lLz) (lHx, lHy, lHz))
    | lHx >= lLx && lHy >= lLy && lHz >= lLz = n
    | otherwise = Empty
simplify (Intersection Empty _) = Empty
simplify (Intersection _ Empty) = Empty
simplify (Intersection (Region (lLx, lLy, lLz) (lHx, lHy, lHz)) (Region (rLx, rLy, rLz) (rHx, rHy, rHz)))
     = simplify (Region (max lLx rLx, max lLy rLy, max lLz rLz) (min lHx rHx, min lHy rHy, min lHz rHz))
simplify (Intersection (DisjointUnion ts) r) = simplify (DisjointUnion [simplify (Intersection t r) | t <- ts])
simplify (Intersection l r@(DisjointUnion _)) = simplify (Intersection r l)
simplify (Intersection (JointDifference il ir) r) = simplify (JointDifference (simplify (Intersection il r)) (simplify (Intersection ir r)))
simplify (Intersection l r@(JointDifference _ _)) = simplify (Intersection r l)
simplify (Intersection l r)
    | nl /= l || nr /= r = simplify (Intersection nl nr)
    | otherwise = error ("Cannot intersect\n" ++ (show l) ++ "\nand\n" ++ (show r))
    where
        nl = simplify l
        nr = simplify r
simplify (Union Empty r) = simplify r
simplify (Union l Empty) = simplify l
simplify (Union l r)
    | i == Empty = simplify (DisjointUnion [nl, nr])
    | otherwise = simplify (DisjointUnion [i, simplify (JointDifference nl i), simplify (JointDifference nr i)])
    where
        nl = simplify l
        nr = simplify r
        i = simplify (Intersection nl nr)
simplify (Difference l Empty) = simplify l
simplify (Difference Empty _) = Empty
simplify (Difference l r)
    | i == Empty = nl
    | otherwise = JointDifference nl i
    where
        nl = simplify l
        i = simplify (Intersection nl (simplify r))
simplify (DisjointUnion ts)
    | null ns = Empty
    | length ns == 1 = head ns
    | otherwise = DisjointUnion ns
    where
        ns = nub . concat $ [case t of {(DisjointUnion n) -> n; Empty -> []; n -> [n]} | t <- ts]
simplify (JointDifference l Empty) = l
simplify (JointDifference Empty _) = Empty
simplify (JointDifference l r)
    | l == r = Empty
    | otherwise = JointDifference l r

size :: BVH -> Integer
size (Empty) = 0
size (DisjointUnion ts) = sum [size t | t <- ts]
size (JointDifference l r) = (size l) - (size r)
size (Region (lLx, lLy, lLz) (lHx, lHy, lHz)) = (lHx - lLx + 1) * (lHy - lLy + 1) * (lHz - lLz + 1)
size n = error ("Cannot calculate size of\n" ++ (show n))

solution :: Day
solution = (
        show . size . simplify . Intersection (Region (-50, -50, -50) (50, 50, 50)) . constructTree . parse parseInput,
        show . size . simplify . constructTree . parse parseInput
    )
