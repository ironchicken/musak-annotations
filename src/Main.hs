module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V
import qualified Graphics.GD as G
import           MuSAK.Annotations.Graph hiding (shapes)
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types
import           System.Environment (getArgs)

loadPage :: FilePath -> IO Page
loadPage f = do
  csvData <- BL.readFile f
  case decode NoHeader csvData of
    Left err    -> error $ "Could not parse " ++ f ++ ": " ++ err
    Right marks -> return $ Page { pg_marks = V.toList marks
                                 , pg_sourceFile = f }

main :: IO ()
main = do
  (a:_) <- getArgs
  p <- loadPage a

  let (g, _, _) = pageGraph p

  mapM_ (\s -> drawShape s >>= G.savePngFile ((sh_label s) ++ ".png")) (shapes p)
