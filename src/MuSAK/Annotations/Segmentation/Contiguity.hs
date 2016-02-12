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
