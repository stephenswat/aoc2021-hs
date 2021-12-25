module Problems.Day23 (solution) where

import Control.Monad.State.Lazy (State, execState, get, modify)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Map (Map, toList, empty, findWithDefault, filter, keys, insert, notMember)

import Common.Algorithm (search)
import Common.Geometry (Point2D, Grid2D, neighbours4, readGrid2D)
import Common.Solution (Day)

data Amphipod
    = Amber
    | Bronze
    | Copper
    | Desert
    deriving (Show, Eq, Ord)

data Tile
    = Empty
    | Wall
    | Amphipod Amphipod
    deriving (Show, Eq, Ord)

type GameState = Grid2D Tile

isOccupied :: Tile -> Bool
isOccupied (Amphipod _) = True
isOccupied _ = False

isAccessible :: Tile -> Bool
isAccessible (Empty) = True
isAccessible _ = False

isComplete :: GameState -> Bool
isComplete g = and
    [ t == Amphipod c || t == Wall
    | (x, c) <- [(3, Amber), (5, Bronze), (7, Copper), (9, Desert)]
    , y <- [2..5]
    , let t = findWithDefault Wall (x, y) g
    ]

parseInput :: String -> GameState
parseInput = fmap parseChar . readGrid2D
    where
        parseChar ' ' = Wall
        parseChar '#' = Wall
        parseChar '.' = Empty
        parseChar 'A' = Amphipod Amber
        parseChar 'B' = Amphipod Bronze
        parseChar 'C' = Amphipod Copper
        parseChar 'D' = Amphipod Desert
        parseChar _   = error "Cannot read character!"

bfs :: Point2D -> GameState -> [(Point2D, Integer)]
bfs c g = Prelude.filter ((/= c) . fst) . toList . execState (go [c]) $ Data.Map.empty
    where
        go :: [Point2D] -> State (Map Point2D Integer) ()
        go [] = return ()
        go (x:xs) = do
            m <- get
            let v = findWithDefault 0 x m
            let n = neighbours4 x
            let f = Prelude.filter ((flip Data.Map.notMember) m) . Prelude.filter (\i -> isAccessible (findWithDefault Wall i g)) $ n

            modify (\i -> foldl (\a n' -> Data.Map.insert n' (v + 1) a) i f)

            go (xs ++ f)

continuations :: GameState -> [(GameState, Integer)]
continuations g =
    [ (ng, cs * (costMod cl))
    | op <- occupied
    , let (Amphipod cl) = findWithDefault Wall op g
    , (np, cs) <- bfs op g
    , np /= (3, 1) && np /= (5, 1) && np /= (7, 1) && np /= (9, 1)
    , if (snd op) == 1 then (snd np) >= 2 else True
    , if (snd op) >= 2 then (snd np) == 1 else True
    , if (snd np) >= 2 then (fst np) == (dstCol cl) else True
    , if (snd op) >= 2 then (fst op) /= (dstCol cl) || (or [t /= Amphipod cl | y <- [2..5], let t = findWithDefault Wall (fst op, y) g]) else True
    , if (snd np) >= 2 then and [t == Amphipod cl || t == Wall | y <- [(1 + snd np )..5], let t = findWithDefault Wall (fst np, y) g] else True
    , let ng = Data.Map.insert op Empty . Data.Map.insert np (Amphipod cl) $ g
    ]
    where
        dstCol Amber  = 3
        dstCol Bronze = 5
        dstCol Copper = 7
        dstCol Desert = 9

        costMod Amber  = 1
        costMod Bronze = 10
        costMod Copper = 100
        costMod Desert = 1000

        occupied = keys . Data.Map.filter isOccupied $ g

play :: GameState -> Integer
play g = snd . fromJust . search isComplete continuations $ g

prepareB :: String -> String
prepareB s = intercalate "\n" (t ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ b)
    where
        (t, b) = splitAt 3 (lines s)

solution :: Day
solution = (
        show . play . parseInput,
        show . play . parseInput . prepareB
    )
