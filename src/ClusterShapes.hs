module Main where

import MuSAK.Annotations.Clustering.KMeans
import MuSAK.Annotations.IOUtils (loadPage)
import MuSAK.Annotations.Segmentation
import MuSAK.Annotations.Types
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.FilePath.Glob (glob)

main :: IO ()
main = do
  args  <- getArgs
  files <- sequence $ map glob args
  allShapes <- mapM collectShapesFromPage (concat files)
  let clusters = cluster (concat allShapes)
  putStrLn $ show clusters

  where
    collectShapesFromPage p = do
      page <- loadPage p
      return $ shapes page
