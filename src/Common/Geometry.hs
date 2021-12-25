module Common.Geometry where

import Data.Map (Map, fromList)

type Point2D = (Integer, Integer)
type Point3D = (Integer, Integer, Integer)

type Grid2D a = Map Point2D a
type Grid3D a = Map Point3D a

neighbours4 :: Point2D -> [Point2D]
neighbours4 (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbours5 :: Point2D -> [Point2D]
neighbours5 p = p:(neighbours4 p)

neighbours8 :: Point2D -> [Point2D]
neighbours8 (x, y) =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0
    ]

neighbours9 :: Point2D -> [Point2D]
neighbours9 p = p:(neighbours8 p)

readGrid2D :: String -> Grid2D Char
readGrid2D s = fromList
    [ ((x, y), c)
    | (y, r) <- zip [0..] (lines s)
    , (x, c) <- zip [0..] r
    ]
