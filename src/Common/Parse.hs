{-# LANGUAGE LambdaCase #-}

module Common.Parse (Parser (Parser), parse) where

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
