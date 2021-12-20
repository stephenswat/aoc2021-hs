{-# LANGUAGE LambdaCase #-}

module Common.Parse (Parser (Parser), parse, read1, read1_, readIf, readExact, readExact_, readOptional) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus, mzero)

newtype Parser f t = Parser ([f] -> [(t, [f])])

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

discard :: Parser a b -> Parser a ()
discard p = do
    _ <- p
    return ()

parse :: Parser f t -> [f] -> t
parse (Parser f) i = fst . head . f $ i

read1_ :: Eq a => a -> Parser a ()
read1_ v = discard $ readIf (== v)

readIf :: (a -> Bool) -> Parser a a
readIf f = Parser g
    where
        g [] = []
        g (x:xs) = if f x then [(x, xs)] else []

read1 :: Parser a a
read1 = Parser (\case {(x:xs) -> [(x, xs)]; [] -> []})

readExact :: Eq a => [a] -> Parser a [a]
readExact q = Parser (f q)
    where
        f [] xs = [(q, xs)]
        f _ [] = []
        f (l:ls) (r:rs)
            | l == r = f ls rs
            | otherwise = []

readExact_ :: Eq a => [a] -> Parser a ()
readExact_ = discard . readExact

readOptional :: Parser a [b] -> Parser a [b]
readOptional p = p <|> Parser (\xs -> [([], xs)])
