module MuSAK.Annotations.Clustering.DistanceThreshold where

import           Data.Map (Map)
import qualified Data.Map as Map
import           MuSAK.Annotations.Similarity.Turning
import           MuSAK.Annotations.Types

groupShapes :: [TurningShape] -> Double -> Map String [Distance] -> TurningShape -> Map String [Distance]
groupShapes trs thresh m tRep@(TurningShape shape _ _) =
  Map.insert (sh_long_label shape) (filter (\d -> (distVal d) < thresh) (map (distance tRep) trs)) m

cluster :: [Shape] -> Map String [Distance]
cluster ss = foldl (groupShapes tReps thresh) Map.empty tReps
  where
    tReps = map turningRep ss
    thresh = 0.01
