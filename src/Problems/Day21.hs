module Problems.Day21 (solution) where

import Control.Monad.State.Lazy (State, evalState, get, modify)
import Data.Maybe (fromJust)
import Data.Map (Map, member, lookup, empty, insert)
import Data.Bifunctor (first, second)

import Common.Solution (Day)
import Common.Parse (Parser, parse, readExact_, readInteger)

data GameState = GameState
    { score :: (Integer, Integer)
    , square :: (Integer, Integer)
    , rolls :: Integer
    , turn :: Bool
    } deriving (Show, Eq, Ord)

readInput :: Parser Char GameState
readInput = do
    readExact_ "Player 1 starting position: "
    p1 <- readInteger
    readExact_ "\nPlayer 2 starting position: "
    p2 <- readInteger
    return GameState { score = (0, 0), square = (p1, p2), rolls = 0, turn = True }

updateState :: Integer -> GameState -> GameState
updateState roll g = g {
        score = playerFn (+ newPlace) . score $ g,
        square = playerFn (const newPlace) . square $ g,
        rolls = (rolls g) + 1,
        turn = not . turn $ g
    }
    where
        oldPlace = if turn g then fst . square $ g else snd . square $ g
        newPlace = ((oldPlace + roll - 1) `mod` 10) + 1
        playerFn = if turn g then first else second

solveA :: GameState -> Integer
solveA g' = (uncurry min . score $ fg) * (rolls fg) * 3
    where
        fg = go g'
        go g
            | ((uncurry max) . score $ g) >= 1000 = g
            | otherwise = go . updateState roll $ g
            where
                roll = ((6 - rolls g - 1) `mod` 10) + 1

solveB :: GameState -> Integer
solveB g' = uncurry max . evalState (go g') $ empty
    where
        ws = [1, 3, 6, 7, 6, 3, 1]
        addWin (al, ar) ((l, r), w) = (al + w * l, ar + w * r)

        go :: GameState -> State (Map GameState (Integer, Integer)) (Integer, Integer)
        go g = do
            if ((uncurry max) . score $ g) >= 21 then do
                return (if (fst . score $ g) >= 21 then (1, 0) else (0, 1))
            else do
                m <- get
                if member g m then do
                    return (fromJust (Data.Map.lookup g m))
                else do
                    rs <- sequence [go (updateState n g) | n <- [3..9]]
                    let v = foldl addWin (0, 0) (zip rs ws)
                    modify (insert g v)
                    return v

solution :: Day
solution = (
        show . solveA . parse readInput,
        show . solveB . parse readInput
    )
