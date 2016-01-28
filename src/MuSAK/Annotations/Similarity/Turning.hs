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
    legs = mapWithPrev leg (Leg 0 0) $ zip (tangent (head (sh_marks s)) : (sh_marks s)) (sh_marks s)
    leg (m1,m2) (Leg prev _) = Leg (turningAngle s prev m1 m2) (markLenScaled s m1)
    tangent m = m { startX = fst (start m)
                  , startY = yMidPoint
                  , endX   = fst (end m)
                  , endY   = yMidPoint }
      where
        yMidPoint = ceiling $ (realToFrac (snd (start m))) + ((realToFrac ((snd (start m)) - (snd (end m)))) / 2.0)

turningAngle :: Shape -> Angle -> Mark -> Mark -> Angle
turningAngle s base m1 m2 = a
  where
    a | theta - base < -pi = theta + 2 * pi
      | theta - base >= pi = theta - 2 * pi
      | otherwise = theta
    theta = atan2 (markLenScaled s m1) (markLenScaled s m2)
