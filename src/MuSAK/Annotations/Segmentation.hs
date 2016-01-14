module MuSAK.Annotations.Segmentation where

import Data.List (groupBy)
import MuSAK.Annotations.Types

distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = ceiling $ sqrt $ width ** 2 + height ** 2
  where
    width  = realToFrac $ abs $ x1 - x2
    height = realToFrac $ abs $ y1 - y2

markLen :: Mark -> Int
markLen m = distance (start m) (end m)

gap :: Mark -> Mark -> Int
gap a b = distance (end a) (start b)

separated :: Mark -> Mark -> Bool
separated a b | a `gap` b >= threshold = True
              | otherwise              = False
  where threshold = 10

contiguous :: Mark -> Mark -> Bool
contiguous a b = not $ separated a b

shapes :: Page -> [Shape]
shapes p = map (\ms -> Shape { sh_marks = ms, sh_label = "" }) $ groupBy contiguous (pg_marks p)
