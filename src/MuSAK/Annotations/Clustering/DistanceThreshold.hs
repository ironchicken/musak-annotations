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

module MuSAK.Annotations.Clustering.DistanceThreshold where

import           Data.Map (Map)
import qualified Data.Map as Map
import           MuSAK.Annotations.Similarity.Turning
import           MuSAK.Annotations.Types

groupShapes :: [TurningShape] -> Double -> Int -> Map String [Distance] -> TurningShape -> Map String [Distance]
groupShapes trs thresh minSize m tRep@(TurningShape shape _ _) =
  if length cluster >= minSize
  then Map.insert (sh_long_label shape) cluster m
  else m
  where
    distanceTuples = map (distance tRep) trs
    cluster        = filter (\d -> (distVal d) < thresh && not (emptyDistance d)) distanceTuples

cluster :: [Shape] -> Map String [Distance]
cluster ss = foldl (groupShapes tReps thresh minSize) Map.empty tReps
  where
    tReps   = map turningRep ss
    thresh  = 0.01
    minSize = 3
