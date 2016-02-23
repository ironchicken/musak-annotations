-- musak-annotations - A tool for analysing the annotations produced
-- by the MuSAK system
--
-- Copyright (C) 2015, 2016 Richard Lewis
-- Author: richard.lewis@gold.ac.uk

-- This file is part of musak-annotations.

-- musak-annotations is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- musak-annotations is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with musak-annotations. If not, see <http://www.gnu.org/licenses/>.

module MuSAK.Annotations.Similarity.Turning where

import MuSAK.Annotations.Geometry hiding (distance)
import MuSAK.Annotations.Types
import Text.Printf

type Angle = Double

data Leg = Leg Angle Length

instance Show Leg where
  show (Leg a l) = "(" ++ (printf "%0.1f" (a * (180 / pi))) ++ ", " ++ (printf "%0.4f" l) ++ ")"

data TurningShape = TurningShape Shape [Leg] Length

instance Show TurningShape where
  show (TurningShape s ls len) = "TurningShape {" ++ (sh_label s) ++ ": " ++ (show ls) ++ "} = " ++ (printf "%0.4f" len)

markLenScaled :: Shape -> Mark -> Length
markLenScaled s m = (markLen m) / (perimeter s)

mapWithPrev :: (a -> b -> b) -> b -> [a] -> [b]
mapWithPrev f prev (x:xs) = let res = f x prev
                            in res : mapWithPrev f res xs
mapWithPrev _ _    []     = []

turningRep :: Shape -> TurningShape
turningRep s = TurningShape s legs (perimeter s)
  where
    legs = mapWithPrev leg (Leg 0 0) $ zip (sh_marks s) ((tail $ sh_marks s) ++ [head $ sh_marks s])
    leg (m1,m2) (Leg prev _) = Leg (turningAngle s prev m1 m2) (markLenScaled s m2)

turningAngle :: Shape -> Angle -> Mark -> Mark -> Angle
turningAngle s base m1 m2 = a
  where
    a | theta - base < -pi = theta + 2 * pi
      | theta - base >= pi = theta - 2 * pi
      | otherwise = theta
    theta = atan2 (markLenScaled s m1) (markLenScaled s m2)

nullTurningShape :: TurningShape
nullTurningShape = TurningShape nullShape [] 0.0

type Distance = (Shape, Shape, Double)

emptyDistance :: Distance -> Bool
emptyDistance (s1, s2, _) = emptyShape s1 && emptyShape s2

distance :: TurningShape -> TurningShape -> Distance
distance (TurningShape sa a _) (TurningShape sb b _) = (sa, sb, sqrt $ abs $ (areaUnder a) ^ 2 - (areaUnder b) ^ 2)
  where
    areaUnder (x:xs) = area x + areaUnder xs
    areaUnder []     = 0.0
    area (Leg a l)   = a * l

distVal :: Distance -> Double
distVal (_, _, val) = val

distanceVal :: TurningShape -> TurningShape -> Double
distanceVal a b = distVal (distance a b)

distEq :: Double -> Distance -> Distance -> Bool
distEq tol (_, _, a) (_, _, b) = tol > abs (a - b)

distCmp :: Distance -> Distance -> Ordering
distCmp (_, _, a) (_, _, b) = a `compare` b

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

instance Eq TurningShape where
  (==) a b = distVal (distance a b) == 0.0

instance Ord TurningShape where
  compare a b = compare aTob bToa
    where
      (_, _, aTob) = distance a b
      (_, _, bToa) = distance b a
