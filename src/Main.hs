module Main where

import           Control.Monad (unless)
import qualified Graphics.GD as G
import qualified Data.Map as Map
import           MuSAK.Annotations.Clustering.DistanceThreshold
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.IOUtils (loadPage, extendBaseName, globExpHome, parseStringList)
import           MuSAK.Annotations.Segmentation.Contiguity
import           MuSAK.Annotations.Types
import           Options.Applicative
import           System.FilePath (takeBaseName)
import           Text.Printf

data Options = DumpShapes DumpShapesOpts
             | OverlayShapes OverlayShapesOpts
             | GenerateClusters ClusterShapesOpts

{---------------------------- DumpShapes ----------------------------------------}

data DumpShapesOpts = DumpShapesOpts {
  dumpCSVFilesPatterns :: [String] } deriving (Eq)

dumpShapesOpts :: Parser Options
dumpShapesOpts =
  DumpShapes
  <$>
  DumpShapesOpts
  <$>
  option (str >>= parseStringList)
      (    short 'a'
        <> metavar "\"FILE...\""
        <> long "annotations"
        <> help "Quoted list of annotations files or patterns." )

dumpShapeFiles :: DumpShapesOpts -> IO ()
dumpShapeFiles o = do
  csvFiles <- sequence $ map globExpHome (dumpCSVFilesPatterns o)

  mapM_ drawShapesFromPage (concat csvFiles)

  where
    drawShapesFromPage p = do
      page <- loadPage p
      let pageName = takeBaseName (pg_sourceFile page)
      unless (emptyPage page) $ mapM_ (drawAndSaveShape pageName) (shapes page)

    drawAndSaveShape pgName s = do
      let fileName = pgName ++ "_" ++ (sh_label s) ++ ".png"
      makeShapeImg s >>= G.savePngFile fileName

{---------------------------- OverlayShapes -------------------------------------}

data OverlayShapesOpts = OverlayShapesOpts {
    overlayCSVFilesPatterns   :: [String]
  , overlayScorePagesPatterns :: [String] } deriving (Eq)

overlayShapesOpts :: Parser Options
overlayShapesOpts =
  OverlayShapes
  <$>
  ( OverlayShapesOpts <$>
    option (str >>= parseStringList)
      (    short 'a'
        <> metavar "\"FILE...\""
        <> long "annotations"
        <> help "Quoted list of annotations files or patterns." )
    <*> option (str >>= parseStringList)
      (    short 's'
        <> metavar "\"FILE...\""
        <> long "scores"
        <> help "Quoted list of score image files or patterns." ) )

overlayShapes :: OverlayShapesOpts -> IO ()
overlayShapes o = do
  csvFiles   <- sequence $ map globExpHome (overlayCSVFilesPatterns o)
  scorePages <- sequence $ map globExpHome (overlayScorePagesPatterns o)

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

{---------------------------- ClusterShapes -------------------------------------}

data ClusterShapesOpts = ClusterShapesOpts {
    clusterCSVFilesPatterns :: [String] } deriving (Eq)

clusterShapesOpts :: Parser Options
clusterShapesOpts =
  GenerateClusters
  <$>
  ClusterShapesOpts
  <$> option (str >>= parseStringList)
      (    short 'a'
        <> metavar "\"FILE...\""
        <> long "annotations"
        <> help "Quoted list of annotations files or patterns." )

clusterShapes :: ClusterShapesOpts -> IO ()
clusterShapes o = do
  csvFiles   <- sequence $ map globExpHome (clusterCSVFilesPatterns o)

  allShapes <- mapM collectShapesFromPage (concat csvFiles)
  let clusters = cluster (concat allShapes)

  putStrLn $ Map.showTreeWith showCluster True True clusters

  where
    collectShapesFromPage p = do
      page <- loadPage p
      return $ shapes page

    showCluster s ds = s ++ (foldl showDistance "" ds)
    showDistance t (_, s, d) = t ++ ", " ++ (sh_long_label s) ++ " (" ++ (printf "%0.4f" d) ++ ")"

config :: Parser Options
config =
  subparser
    ( command "dump-shapes" ( info dumpShapesOpts
        ( progDesc "Dump individual image files for all the shapes found in the given CSV source files." ))
      <> command "overlay-shapes" ( info overlayShapesOpts
        ( progDesc "Overlay the shapes found in the given CSV source files onto the given score page images creating new images for each score page." ))
      <> command "cluster-shapes" ( info clusterShapesOpts
        ( progDesc "Generate clusters of similar shapes found in the given CSV source files using the given similarity measure." ))
    )

opts :: ParserInfo Options
opts = info ( helper <*> config )
       ( fullDesc
         <> progDesc "MuSAK annotations analysis."
         <> header "musak-annotations" )

dispatchCommand :: Options -> IO ()
dispatchCommand o = case o of
  (DumpShapes os)       -> dumpShapeFiles os
  (OverlayShapes os)    -> overlayShapes os
  (GenerateClusters os) -> clusterShapes os
  _                     -> fail "Unrecognised command"

main :: IO ()
main = execParser opts >>= dispatchCommand

