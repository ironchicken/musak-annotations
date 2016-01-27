module Main where

import           Control.Monad (unless)
import qualified Graphics.GD as G
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.IOUtils (loadPage)
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types
import           System.Environment (getArgs)
import           System.FilePath (takeBaseName)
import           System.FilePath.Glob (glob)

main :: IO ()
main = do
  args  <- getArgs
  files <- sequence $ map glob args

  mapM_ drawShapesFromPage (concat files)

  where
    drawShapesFromPage p = do
      page <- loadPage p
      let pageName = takeBaseName (pg_sourceFile page)
      unless (emptyPage page) $ mapM_ (drawAndSaveShape pageName) (shapes page)

    drawAndSaveShape pgName s = do
      let fileName = pgName ++ "_" ++ (sh_label s) ++ ".png"
      makeShapeImg s >>= G.savePngFile fileName
