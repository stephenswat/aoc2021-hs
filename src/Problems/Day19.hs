module Problems.Day19 (solution) where

import Control.Applicative (Alternative (many))
import Data.List (maximumBy)
import Data.Function (on)
import Data.Set (Set, fromList, intersection, toList, map, size, union, insert, singleton, empty)
import Data.MultiSet (fromList, occur)

import Common.Parse (Parser, parse, read1_, readIf, readExact_, readInteger)
import Common.Solution (Day)

type Coordinate = (Integer, Integer, Integer)

transforms :: [Coordinate -> Coordinate]
transforms =
    [ \(x, y, z) -> (x, y, z)
    , \(x, y, z) -> (x, -z, y)
    , \(x, y, z) -> (x, -y, -z)
    , \(x, y, z) -> (x, z, -y)

    , \(x, y, z) -> (-x, -y, z)
    , \(x, y, z) -> (-x, -z, -y)
    , \(x, y, z) -> (-x, y, -z)
    , \(x, y, z) -> (-x, z, y)

    , \(x, y, z) -> (y, -x, z)
    , \(x, y, z) -> (y, -z, -x)
    , \(x, y, z) -> (y, x, -z)
    , \(x, y, z) -> (y, z, x)

    , \(x, y, z) -> (-y, x, z)
    , \(x, y, z) -> (-y, -z, x)
    , \(x, y, z) -> (-y, -x, -z)
    , \(x, y, z) -> (-y, z, -x)

    , \(x, y, z) -> (z, x, y)
    , \(x, y, z) -> (z, y, -x)
    , \(x, y, z) -> (z, -x, -y)
    , \(x, y, z) -> (z, -y, x)

    , \(x, y, z) -> (-z, y, x)
    , \(x, y, z) -> (-z, -x, y)
    , \(x, y, z) -> (-z, -y, -x)
    , \(x, y, z) -> (-z, x, -y)
    ]

parseCoordinate :: Parser Char Coordinate
parseCoordinate = do
    p1 <- readInteger
    read1_ ','
    p2 <- readInteger
    read1_ ','
    p3 <- readInteger
    return (p1, p2, p3)

parseScanner :: Parser Char [Coordinate]
parseScanner = do
    readExact_ "--- scanner "
    _ <- readInteger
    readExact_ " ---\n"
    r <- many (do
        r <- parseCoordinate
        read1_ '\n'
        return r)
    return r

parseInput :: Parser Char [[Coordinate]]
parseInput = do
    rs <- many (do
        r <- parseScanner
        read1_ '\n'
        return r)
    rl <- parseScanner
    return (rs ++ [rl])

distances :: [Coordinate] -> Set Coordinate
distances c = Data.Set.fromList
    [ (x1 - x2, y1 - y2, z1 - z2)
    | p1@(x1, y1, z1) <- c
    , p2@(x2, y2, z2) <- c
    , p1 /= p2
    ]

solve :: [[Coordinate]] -> (Set Coordinate, Set Coordinate)
solve [] = (empty, empty)
solve (i:is) = go is (Data.Set.fromList i) (distances i) (singleton (0, 0, 0))
    where
        go [] b _ s = (b, s)
        go ss b d s = go (filter (/= c) ss) (union b nns) (union d nd) (insert tb s)
            where
                q =
                    [ (f', s', d')
                    | f' <- transforms
                    , s' <- ss
                    , let d' = distances $ (fmap f' s')
                    , let i' = intersection d d'
                    , size i' >= 12
                    ]
                (f, c, nd) = head q
                r = Data.MultiSet.fromList
                    [ (x1 - x2, y1 - y2, z1 - z2)
                    | (x1, y1, z1) <- toList b
                    , p2 <- c
                    , let (x2, y2, z2) = f p2
                    ]
                tb@(tx, ty, tz) = maximumBy (compare `on` (\x -> occur x r)) r
                nns = Data.Set.map ((\(x, y, z) -> (x + tx, y + ty, z + tz)) . f) . Data.Set.fromList $ c

cartManhattan :: Set Coordinate -> Set Integer
cartManhattan s = Data.Set.fromList
    [ (abs (x1 - x2)) + (abs (y1 - y2)) + (abs z1 - z2)
    | (x1, y1, z1) <- toList s
    , (x2, y2, z2) <- toList s
    ]

solution :: Day
solution = (
        show . size . fst . solve . parse parseInput,
        show . maximum . cartManhattan . snd . solve . parse parseInput
    )
