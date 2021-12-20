module Problems.Day19 (solution) where

import Control.Applicative (Alternative (some, many))
import Data.List (maximumBy)
import Data.Function (on)
import Data.Char (isDigit)
import Data.Set (Set, fromList, intersection, toList, map, size, union, insert, singleton, empty)
import Data.MultiSet (fromList, occur)

import Common.Parse (Parser, parse, read1_, readIf, readExact, readExact_, readOptional)
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


parseNumber :: Parser Char Integer
parseNumber = do
    s <- readOptional (readExact "-")
    n <- some (readIf isDigit)
    return (read (s ++ n))

parseCoordinate :: Parser Char Coordinate
parseCoordinate = do
    p1 <- parseNumber
    read1_ ','
    p2 <- parseNumber
    read1_ ','
    p3 <- parseNumber
    return (p1, p2, p3)

parseScanner :: Parser Char (Set Coordinate)
parseScanner = do
    readExact_ "--- scanner "
    _ <- some (readIf isDigit)
    readExact_ " ---\n"
    r <- many (do
        r <- parseCoordinate
        read1_ '\n'
        return r)
    return (Data.Set.fromList r)

parseInput :: Parser Char [Set Coordinate]
parseInput = do
    rs <- many (do
        r <- parseScanner
        read1_ '\n'
        return r)
    rl <- parseScanner
    return (rs ++ [rl])

distances :: Set Coordinate -> Set Coordinate
distances c = Data.Set.fromList
    [ (x1 - x2, y1 - y2, z1 - z2)
    | p1@(x1, y1, z1) <- (toList c)
    , p2@(x2, y2, z2) <- (toList c)
    , p1 /= p2
    ]

solve :: [Set Coordinate] -> (Set Coordinate, Set Coordinate)
solve [] = (empty, empty)
solve (i:is) = go is i (singleton (0, 0, 0))
    where
        go [] b s = (b, s)
        go ss b s = go (filter (/= s2) ss) (union b nns) (insert tb s)
            where
                q =
                    [ (f', s')
                    | f' <- transforms
                    , s' <- ss
                    ]
                (f, s2) = maximumBy (compare `on` (size . (\(f', s') -> intersection (distances b) (distances (Data.Set.map f' s'))))) q
                r = Data.MultiSet.fromList
                    [ (x1 - x2, y1 - y2, z1 - z2)
                    | (x1, y1, z1) <- toList b
                    , p2 <- toList s2
                    , let (x2, y2, z2) = f p2
                    ]
                tb@(tx, ty, tz) = maximumBy (compare `on` (\x -> occur x r)) r
                nns = Data.Set.map ((\(x, y, z) -> (x + tx, y + ty, z + tz)) . f) s2

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
