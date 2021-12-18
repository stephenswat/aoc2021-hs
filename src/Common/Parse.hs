{-# LANGUAGE LambdaCase #-}

module Common.Parse (Parser (Parser), parse, read1, read1_, readIf) where

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

parse :: Parser f t -> [f] -> t
parse (Parser f) i = fst . head . f $ i

read1_ :: Eq a => a -> Parser a ()
read1_ v = Parser (\(x:xs) -> if x == v then [((), xs)] else [])

readIf :: (a -> Bool) -> Parser a a
readIf f = Parser (\(x:xs) -> if f x then [(x, xs)] else [])

read1 :: Parser a a
read1 = Parser (\case {(x:xs) -> [(x, xs)]; [] -> []})
