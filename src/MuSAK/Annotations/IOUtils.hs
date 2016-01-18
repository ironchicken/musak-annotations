module MuSAK.Annotations.IOUtils (loadPage) where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V
import           MuSAK.Annotations.Types

loadPage :: FilePath -> IO Page
loadPage f = do
  csvData <- BL.readFile f
  case decode NoHeader csvData of
    Left err    -> error $ "Could not parse " ++ f ++ ": " ++ err
    Right marks -> return $ Page { pg_marks = V.toList marks
                                 , pg_sourceFile = f }
