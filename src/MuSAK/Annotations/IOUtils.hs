-- musak-annotations - A tool for analysing the annotations produced
-- by the MuSAK system
--
-- Copyright (C) 2015, 2016 Richard Lewis
-- Author: richard.lewis@gold.ac.uk

-- This file is part of musak-annotations.

-- musak-annotations is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- musak-annotations is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with musak-annotations. If not, see <http://www.gnu.org/licenses/>.

module MuSAK.Annotations.IOUtils ( loadPage
                                 , extendBaseName
                                 , globExpHome
                                 , parseStringList ) where

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

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words
