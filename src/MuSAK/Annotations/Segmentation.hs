module MuSAK.Annotations.Segmentation where

import MuSAK.Annotations.Types
import Text.Printf (printf)

distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = ceiling $ sqrt $ width ** 2 + height ** 2
  where
    width  = (realToFrac $ abs $ x1 - x2) :: Double
    height = (realToFrac $ abs $ y1 - y2) :: Double

markLen :: Mark -> Int
markLen m = distance (start m) (end m)

gap :: Mark -> Mark -> Int
gap a b = distance (end a) (start b)

separated :: Mark -> Mark -> Bool
separated a b | a `gap` b >= threshold = True
              | otherwise              = False
  where threshold = 100

contiguous :: Mark -> Mark -> Bool
contiguous a b = not $ separated a b

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p xs = reverse $ makeGroups xs [[]]
  where
    makeGroups (x:y:rest) acc = if p x y
                                then makeGroups (y:rest) ((x:(head acc)):(tail acc))
                                else makeGroups rest ([y]:((x:(head acc)):(tail acc)))
    makeGroups (y:rest) acc   = (y:(head acc)):(tail acc)
    makeGroups []       acc   = acc

shapes :: Page -> [Shape]
shapes p = map (\(ms,i) -> Shape { sh_marks = ms, sh_label = "shape" ++ (printf "%02d" i) }) $ zip (groupBy contiguous (pg_marks p)) [(0 :: Int)..]
