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

module MuSAK.Annotations.Geometry where

import Data.List (minimum, maximum)
import MuSAK.Annotations.Types

bounds :: Shape -> (Int, Int, Int, Int)
bounds s = (left, top, right, bottom)
  where
    xs = concat $ map (\m -> [fst (start m), fst (end m)]) (sh_marks s)
    ys = concat $ map (\m -> [snd (start m), snd (end m)]) (sh_marks s)
    left   = minimum xs
    right  = maximum xs
    top    = minimum ys
    bottom = maximum ys

sizeOf :: Shape -> (Int, Int)
sizeOf s = (max 1 (right - left), max 1 (bottom - top))
  where
    (left, top, right, bottom) = bounds s

offset :: Shape -> Point
offset s = (left, top)
  where
    (left, top, _, _) = bounds s

translateToO :: Point -> Point -> Point
translateToO (x1,y1) (offsX,offsY) = (x1 - offsX, y1 - offsY)

distance :: Point -> Point -> Length
distance (x1,y1) (x2,y2) = sqrt $ width ** 2 + height ** 2
  where
    width  = (realToFrac $ abs $ x1 - x2) :: Double
    height = (realToFrac $ abs $ y1 - y2) :: Double

markLen :: Mark -> Length
markLen m = distance (start m) (end m)

distanceInt :: Point -> Point -> Int
distanceInt a b = ceiling $ distance a b

markLenInt :: Mark -> Int
markLenInt m = distanceInt (start m) (end m)

perimeterWithLen :: (Num a) => (Mark -> a) -> Shape -> a
perimeterWithLen lenF s = foldl (+) 0 $ (map lenF (sh_marks s))

perimeter :: Shape -> Length
perimeter = perimeterWithLen markLen

perimeterInt :: Shape -> Int
perimeterInt = perimeterWithLen markLenInt
