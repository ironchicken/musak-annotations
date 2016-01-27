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
