module Main where

import           Control.Monad (unless)
import qualified Graphics.GD as G
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.IOUtils (loadPage, extendBaseName, globExpHome)
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types
import           Options.Applicative

data SourceFiles = SourceFiles {
    csvFilesPatterns   :: [String]
  , scorePagesPatterns :: [String] }

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

config :: Parser SourceFiles
config = SourceFiles
         <$> option (str >>= parseStringList)
             (    short 'a'
               <> metavar "\"FILE...\""
               <> long "annotations"
               <> help "Quoted list of annotations files or patterns." )
         <*> option (str >>= parseStringList)
             (    short 's'
               <> metavar "\"FILE...\""
               <> long "scores"
               <> help "Quoted list of score image files or patterns." )

opts :: ParserInfo SourceFiles
opts = info ( helper <*> config )
       ( fullDesc
         <> progDesc "Render MuSAK annotations onto score pages."
         <> header "musak-highlight-shapes" )

renderPages :: SourceFiles -> IO ()
renderPages srcs = do
  csvFiles   <- sequence $ map globExpHome (csvFilesPatterns srcs)
  scorePages <- sequence $ map globExpHome (scorePagesPatterns srcs)

  mapM_ drawShapesOntoPage $ zip (concat scorePages) (concat csvFiles)

  where
    drawShapesOntoPage (scorePage, annots) = do
      page <- loadPage annots
      let newFileName = extendBaseName scorePage "_withShapes"
      putStrLn $ "(" ++ scorePage ++ ", " ++ annots ++ ") -> " ++ newFileName
      unless (emptyPage page) $ do
        img <- makePageImgWithShapes scorePage page
        G.saveJpegFile quality newFileName img

    quality = 95

main :: IO ()
main = execParser opts >>= renderPages
