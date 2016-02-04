module MuSAK.Annotations.Clustering.KMeans where

-- k-Means implementation adapted from <https://github.com/BinRoot/Haskell-Data-Analysis-Cookbook/blob/master/Ch08/Code01_kmeans/Main.hs>

import           Data.List (minimumBy, sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import           MuSAK.Annotations.Similarity.Turning
import           MuSAK.Annotations.Types

-- Assign points to closest centroids
assign :: [TurningShape] -> [TurningShape] -> Map TurningShape [TurningShape]
assign centroids tReps = Map.fromListWith (++)
                          [(assignTRep t, [t]) | t <- tReps]
  where assignTRep t = minimumBy (comparing (distanceVal t)) centroids

-- Relocate centroids to the middle of its group
relocate :: Map TurningShape [TurningShape] -> Map TurningShape [TurningShape]
relocate centroidsMap = Map.foldrWithKey insertCentre Map.empty centroidsMap
  where insertCentre _ ts m = Map.insert (centre ts) ts m
        centre [] = nullTurningShape
        centre ts = head $ snd $ splitAt ((length ts) `div` 2) ts

-- Assign turning represetations and relocate centroids until centroids converge
kmeans :: [TurningShape] -> [TurningShape] -> [TurningShape]
kmeans centroids tReps =
  if converged then centroids else kmeans (Map.keys newCentroidsMap) tReps
    where converged = all (< 1e-4) $
                      zipWith distanceVal (sort centroids) (Map.keys newCentroidsMap)
          newCentroidsMap = relocate (assign centroids tReps)

cluster :: [Shape] -> [TurningShape]
cluster shapes =
  let tReps = map turningRep shapes
  in kmeans (take 10 tReps) tReps
