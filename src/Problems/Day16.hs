{-# LANGUAGE LambdaCase #-}

module Problems.Day16 (solution) where

import Control.Applicative (Alternative (empty, (<|>)), some)
import Control.Monad (MonadPlus, replicateM, mzero)

import Common.Solution (Day)

newtype Parser f t = Parser ([f] -> [(t, [f])])

data Packet
    = Literal Int Int
    | Operator Int ([Int] -> Int) [Packet]

instance Functor (Parser f) where
    fmap f (Parser g) = Parser (\i -> [(f x, xs) | (x, xs) <- g i])

instance Applicative (Parser f) where
    pure v = Parser (\i -> [(v, i)])
    (<*>) (Parser f) (Parser g) = Parser (\i -> [(f' v, xss) | (f', xs) <- f i, (v, xss) <- g xs])

instance Alternative (Parser f) where
    empty = Parser (\_ -> [])
    (<|>) (Parser f) (Parser g) = Parser (\i -> (f i) ++ (g i))

instance Monad (Parser f) where
    (>>=) (Parser f) g = Parser (\i -> concat [a' xs | (a, xs) <- f i, let (Parser a') = g a])

instance MonadPlus (Parser f)

instance MonadFail (Parser f) where
    fail _ = mzero

toDecimal :: [Bool] -> Int
toDecimal b = go . reverse $ b
    where
        go [] = 0
        go (x:xs) = (if x then 1 else 0) + 2 * (go xs)

parseBit :: Parser Bool Bool
parseBit = Parser (\case {(x:xs) -> [(x, xs)]; [] -> []})

readBits :: Int -> Parser Bool [Bool]
readBits n = Parser (return . splitAt n)

parse :: Parser f t -> [f] -> t
parse (Parser f) i = fst . head . f $ i

parseLiteralBits :: Parser Bool [Bool]
parseLiteralBits = do
    (b:bs) <- readBits 5
    if (b) then do
        r <- parseLiteralBits
        return (bs ++ r)
    else
        return bs

parseLiteral :: Int -> Parser Bool Packet
parseLiteral v = do
    b <- parseLiteralBits
    return (Literal v (toDecimal b))

parseOperator :: ([Packet] -> Packet) -> Parser Bool Packet
parseOperator t = do
    l <- parseBit
    if l then do
        c <- fmap toDecimal (readBits 11)
        p <- replicateM c parsePacket
        return (t p)
    else do
        c <- fmap toDecimal (readBits 15)
        b <- readBits c
        return (t (parse (some parsePacket) $ b))

parsePacket :: Parser Bool Packet
parsePacket = do
    v <- fmap toDecimal (readBits 3)
    t <- fmap toDecimal (readBits 3)
    case t of
        0 -> parseOperator (Operator v sum)
        1 -> parseOperator (Operator v product)
        2 -> parseOperator (Operator v minimum)
        3 -> parseOperator (Operator v maximum)
        4 -> parseLiteral v
        5 -> parseOperator (Operator v (\(p1:p2:_) -> if p1 > p2 then 1 else 0))
        6 -> parseOperator (Operator v (\(p1:p2:_) -> if p1 < p2 then 1 else 0))
        7 -> parseOperator (Operator v (\(p1:p2:_) -> if p1 == p2 then 1 else 0))
        _ -> error "Invalid operator ID!"

toBinary :: Char -> [Bool]
toBinary '0' = [False, False, False, False]
toBinary '1' = [False, False, False,  True]
toBinary '2' = [False, False,  True, False]
toBinary '3' = [False, False,  True,  True]
toBinary '4' = [False,  True, False, False]
toBinary '5' = [False,  True, False,  True]
toBinary '6' = [False,  True,  True, False]
toBinary '7' = [False,  True,  True,  True]
toBinary '8' = [ True, False, False, False]
toBinary '9' = [ True, False, False,  True]
toBinary 'A' = [ True, False,  True, False]
toBinary 'B' = [ True, False,  True,  True]
toBinary 'C' = [ True,  True, False, False]
toBinary 'D' = [ True,  True, False,  True]
toBinary 'E' = [ True,  True,  True, False]
toBinary 'F' = [ True,  True,  True,  True]
toBinary _   = []

solveA :: Packet -> Int
solveA (Literal v _) = v
solveA (Operator v _ ps) = v + sum [solveA p | p <- ps]

solveB :: Packet -> Int
solveB (Literal _ v) = v
solveB (Operator _ f ps) = f [solveB p | p <- ps]

solution :: Day
solution = (
        show . solveA . parse parsePacket . concatMap toBinary,
        show . solveB . parse parsePacket . concatMap toBinary
    )
