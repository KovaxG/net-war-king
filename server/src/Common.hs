module Common where

import Types.Common

(-->) :: a -> b -> (a,b)
(-->) = (,)

-- Manhattan Distance
manDist :: Coord -> Coord -> Int
manDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

relativeTo :: Coord -> Coord -> Coord
relativeTo (x0, y0) (x,y) = (x-x0, y-y0)
