module Main where

import qualified Data.Map as Map
import           MuSAK.Annotations.Clustering.DistanceThreshold
import           MuSAK.Annotations.IOUtils (loadPage)
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Similarity.Turning
import           MuSAK.Annotations.Types
import           System.Environment (getArgs)
import           System.FilePath (takeBaseName)
import           System.FilePath.Glob (glob)
import           Text.Printf

main :: IO ()
main = do
  args  <- getArgs
  files <- sequence $ map glob args
  allShapes <- mapM collectShapesFromPage (concat files)
  let clusters = cluster (concat allShapes)

  putStrLn $ Map.showTreeWith showCluster True True clusters

  where
    collectShapesFromPage p = do
      page <- loadPage p
      return $ shapes page

    showCluster s ds = s ++ (foldl showDistance "" ds)
    showDistance t (_, s, d) = t ++ ", " ++ (sh_long_label s) ++ " (" ++ (printf "%0.4f" d) ++ ")"
