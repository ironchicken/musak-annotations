module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V
import           MuSAK.Annotations.Graph hiding (shapes)
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
  putStrLn $ show p
  let (g, _, _) = pageGraph p
  putStrLn $ show g

  let s = shapes p
  putStrLn $ show s
