module MuSAK.Annotations.IOUtils ( loadPage
                                 , extendBaseName
                                 , globExpHome ) where

import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.List (sort)
import qualified Data.Vector as V
import           MuSAK.Annotations.Types
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.FilePath (joinPath, splitExtension, (<.>))
import           System.FilePath.Glob (glob)

loadPage :: FilePath -> IO Page
loadPage f = do
  e <- doesFileExist f
  unless e $ fail $ f ++ " does not exist."
  csvData <- BL.readFile f
  case decode NoHeader csvData of
    Left err    -> error $ "Could not parse " ++ f ++ ": " ++ err
    Right marks -> return $ Page { pg_marks = V.toList marks
                                 , pg_sourceFile = f }

extendBaseName :: FilePath -> String -> FilePath
extendBaseName fp add = (front ++ add) <.> ext
  where
    (front, ext) = splitExtension fp

globExpHome :: String -> IO [FilePath]
globExpHome p = do
  home  <- getHomeDirectory
  paths <- glob $ fullPath home p
  return $ sort paths
  where
    fullPath h ('~':'/':s) = joinPath [h, s]
    fullPath h ('~':s)     = joinPath [h, s]
    fullPath _ s           = s
