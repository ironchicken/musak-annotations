module Main where

import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V
import qualified Graphics.GD as G
import           MuSAK.Annotations.Graph hiding (shapes)
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types
import           System.Environment (getArgs)
import           System.FilePath (takeBaseName)
import           System.FilePath.Glob (glob)

loadPage :: FilePath -> IO Page
loadPage f = do
  csvData <- BL.readFile f
  case decode NoHeader csvData of
    Left err    -> error $ "Could not parse " ++ f ++ ": " ++ err
    Right marks -> return $ Page { pg_marks = V.toList marks
                                 , pg_sourceFile = f }

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
      drawShape s >>= G.savePngFile fileName
