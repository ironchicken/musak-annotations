module MuSAK.Annotations.Segmentation where

import MuSAK.Annotations.Geometry
import MuSAK.Annotations.Types
import Text.Printf (printf)

gap :: Mark -> Mark -> Int
gap a b = distanceInt (end a) (start b)

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
