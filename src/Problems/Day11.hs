module Problems.Day11 (solution) where

import Data.Set (empty, insert, toList, (\\), union)
import Data.Map (elems, foldrWithKey, adjust, foldr, map)

import Common.Solution (Day)
import Common.Geometry (Grid2D, neighbours9, readGrid2D)

step :: Grid2D Integer -> Grid2D Integer
step = fmap cap . (midStep empty) . fmap (+ 1)
    where
        midStep s g
            | n == g = g
            | otherwise = midStep (union s flashes) n
            where
                flashes = foldrWithKey (\k v l -> if v >= 10 then insert k l else l) empty g
                n = Prelude.foldr (adjust (+1)) g . concat . fmap neighbours9 . toList . (\\ s) $ flashes
        cap x = if x >= 10 then 0 else x

solution :: Day
solution = (
        solve (sum . fmap (toInteger . length . filter (== 0) . elems) . take 101),
        solve (fst . head . filter ((== 0) . Data.Map.foldr (+) 0 . snd) . zip [0..])
    )
    where
        solve f = (show :: (Integer -> String)) . f . iterate step . Data.Map.map (\x -> read [x]) . readGrid2D
