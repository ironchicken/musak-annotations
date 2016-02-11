module MuSAK.Annotations.Segmentation.Contiguity (shapes) where

import MuSAK.Annotations.Geometry
import MuSAK.Annotations.Types
import Text.Printf (printf)

gap :: Mark -> Mark -> Int
gap a b = distanceInt (end a) (start b)

separated :: Int -> Mark -> Mark -> Bool
separated threshold a b | a `gap` b >= threshold = True
                        | otherwise              = False

contiguous :: Int -> Mark -> Mark -> Bool
contiguous threshold a b = not $ separated threshold a b

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p xs = reverse $ makeGroups xs [[]]
  where
    makeGroups (x:y:rest) acc = if p x y
                                then makeGroups (y:rest) ((x:(head acc)):(tail acc))
                                else makeGroups rest ([y]:((x:(head acc)):(tail acc)))
    makeGroups (y:rest) acc   = (y:(head acc)):(tail acc)
    makeGroups []       acc   = acc

shapes :: Page -> Options -> [Shape]
shapes p opts =
  map (\(ms,i) ->
        Shape { sh_marks = ms
              , sh_long_label = (pg_sourceFile p) ++ "_shape" ++ (printf "%02d" i)
              , sh_label = "shape" ++ (printf "%02d" i) } )
  $ zip (groupBy (contiguous (contiguityThreshold opts)) (pg_marks p)) [(0 :: Int)..]
