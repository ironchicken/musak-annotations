module MuSAK.Annotations.Similarity.Turning where

import MuSAK.Annotations.Geometry
import MuSAK.Annotations.Types

type Angle = Double

data Leg = Leg Angle Length deriving Show

data TurningShape = TurningShape Shape [Leg] Length deriving Show

markLenScaled :: Shape -> Mark -> Length
markLenScaled s m = (markLen m) / (perimeter s)

mapWithPrev :: (a -> b -> b) -> b -> [a] -> [b]
mapWithPrev f prev (x:xs) = let res = f x prev
                            in res : mapWithPrev f res xs
mapWithPrev _ _    []     = []

turningRep :: Shape -> TurningShape
turningRep s = TurningShape s legs (perimeter s)
  where
    legs = mapWithPrev leg (Leg 0 0) $ zip (tangent (head (sh_marks s)) : (sh_marks s)) ((sh_marks s) ++ [tangent (head (sh_marks s))])
    leg (m1,m2) (Leg prev _) = Leg (turningAngle s prev m1 m2) (markLenScaled s m2)
    tangent m = m { startX = (fst (start m)) - 5
                  , startY = midPoint (startY m) (endY m)
                  , endX   = (fst (end m)) + 5
                  , endY   = midPoint (startY m) (endY m) }
      where
        midPoint :: Int -> Int -> Int
        midPoint y1 y2 | y1 < y2 = y1 + ((y2 - y1) `div` 2)
                       | y1 > y2 = y2 + ((y1 - y2) `div` 2)
                       | otherwise = y1

turningAngle :: Shape -> Angle -> Mark -> Mark -> Angle
turningAngle s base m1 m2 = a
  where
    a | theta - base < -pi = theta + 2 * pi
      | theta - base >= pi = theta - 2 * pi
      | otherwise = theta
    theta = atan2 (markLenScaled s m1) (markLenScaled s m2)

type Distance = (Shape, Shape, Double)

distance :: TurningShape -> TurningShape -> Distance
distance (TurningShape sa a _) (TurningShape sb b _) = (sa, sb, sqrt $ abs $ (areaUnder a) ^ 2 - (areaUnder b) ^ 2)
  where
    areaUnder (x:xs) = area x + areaUnder xs
    areaUnder []     = 0.0
    area (Leg a l)   = a * l

instance Eq Shape where
  (==) a b = dist == 0.0
    where (_, _, dist) = distance (turningRep a) (turningRep b)

instance Ord Shape where
  compare a b = compare aTob bToa
    where
      aT = (turningRep a)
      bT = (turningRep b)
      (_, _, aTob) = distance aT bT
      (_, _, bToa) = distance bT aT
