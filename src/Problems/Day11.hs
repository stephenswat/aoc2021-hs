module Problems.Day11 (solution) where

import Data.Set (empty, insert, toList, (\\), union)
import Data.Map (Map, fromList, elems, foldrWithKey, adjust, foldr)

import Common.Solution (Day)

type Coordinate = (Integer, Integer)
type Grid = Map Coordinate Integer

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

readGrid :: String -> Grid
readGrid s = fromList $ [((x, y), read [c]) | (y, r) <- zip [0..] (lines s), (x, c) <- zip [0..] r]

step :: Grid -> Grid
step = fmap cap . (midStep empty) . fmap (+ 1)
    where
        midStep s g
            | n == g = g
            | otherwise = midStep (union s flashes) n
            where
                flashes = foldrWithKey (\k v l -> if v >= 10 then insert k l else l) empty g
                n = Prelude.foldr (adjust (+1)) g . concat . fmap neighbours . toList . (\\ s) $ flashes
        cap x = if x >= 10 then 0 else x

solution :: Day
solution = (
        solve (sum . fmap (toInteger . length . filter (== 0) . elems) . take 101),
        solve (fst . head . filter ((== 0) . Data.Map.foldr (+) 0 . snd) . zip [0..])
    )
    where
        solve f = (show :: (Integer -> String)) . f . iterate step . readGrid
