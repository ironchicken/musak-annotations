module Main where

import           Control.Monad (unless)
import qualified Graphics.GD as G
import qualified Data.Map as Map
import           MuSAK.Annotations.Clustering.DistanceThreshold
import           MuSAK.Annotations.Drawing
import           MuSAK.Annotations.IOUtils (loadPage, extendBaseName, globExpHome, parseStringList)
import           MuSAK.Annotations.Segmentation (segmenters)
import           MuSAK.Annotations.Types
import           Options.Applicative
import           System.FilePath (takeBaseName)
import           Text.Printf

annotationsFilesOpt :: Parser [String]
annotationsFilesOpt = option (str >>= parseStringList)
  (    short 'a'
    <> metavar "\"FILE...\""
    <> long "annotations"
    <> help "Quoted list of annotations files or patterns." )

scoreFilesOpt :: Parser [String]
scoreFilesOpt = option (str >>= parseStringList)
  (    short 's'
    <> metavar "\"FILE...\""
    <> long "scores"
    <> help "Quoted list of score image files or patterns." )

segmenterOpt :: Parser String
segmenterOpt = strOption
  (    long "segmenter"
    <> metavar "SEGMENTER"
    <> value "contiguity"
    <> help "Name of segmentation algorithm used to segment page marks into distinct shapes. Options: 'contiguity'." )

parseInt :: Monad m => String -> m Int
parseInt = return . read

contiguityThresholdOpt :: Parser Int
contiguityThresholdOpt = option (str >>= parseInt)
  (    long "contiguity-threshold"
    <> metavar "CONTIG-THRESHOLD"
    <> value 100
    <> help "Largest permissible gap between two marks in a shape. Gaps larger than this determine shape boundaries." )

{---------------------------- DumpShapes ----------------------------------------}

dumpShapesOpts :: Parser Options
dumpShapesOpts = DumpShapes <$> (DumpShapesOpts <$> annotationsFilesOpt <*> segmenterOpt <*> contiguityThresholdOpt)

dumpShapeFiles :: Options -> DumpShapesOpts -> IO ()
dumpShapeFiles optns o = do
  csvFiles <- sequence $ map globExpHome (dumpCSVFilesPatterns o)

  mapM_ (drawShapesFromPage (lookup (dumpSegmenter o) segmenters)) (concat csvFiles)

  where
    drawShapesFromPage Nothing _       = fail "Unknown segmenter"
    drawShapesFromPage (Just shapesIn) p = do
      page <- loadPage p
      let pageName = takeBaseName (pg_sourceFile page)
      unless (emptyPage page) $ mapM_ (drawAndSaveShape pageName) (shapesIn page optns)

    drawAndSaveShape pgName s = do
      let fileName = pgName ++ "_" ++ (sh_label s) ++ ".png"
      makeShapeImg s >>= G.savePngFile fileName

{---------------------------- OverlayShapes -------------------------------------}

overlayShapesOpts :: Parser Options
overlayShapesOpts = OverlayShapes <$>
  ( OverlayShapesOpts
    <$> annotationsFilesOpt
    <*> scoreFilesOpt
    <*> segmenterOpt
    <*> contiguityThresholdOpt )

overlayShapes :: Options -> OverlayShapesOpts -> IO ()
overlayShapes optns o = do
  csvFiles   <- sequence $ map globExpHome (overlayCSVFilesPatterns o)
  scorePages <- sequence $ map globExpHome (overlayScorePagesPatterns o)

  mapM_ (drawShapesOntoPage (lookup (overlaySegmenter o) segmenters)) $
    zip (concat scorePages) (concat csvFiles)

  where
    drawShapesOntoPage Nothing _                         = fail "Unkown segmenter"
    drawShapesOntoPage (Just shapesIn) (scorePage, annots) = do
      page <- loadPage annots
      let newFileName = extendBaseName scorePage "_withShapes"
      putStrLn $ "(" ++ scorePage ++ ", " ++ annots ++ ") -> " ++ newFileName
      unless (emptyPage page) $ do
        img <- makePageImgWithShapes shapesIn scorePage page optns
        G.saveJpegFile quality newFileName img

    quality = 95

{---------------------------- ClusterShapes -------------------------------------}

clusterShapesOpts :: Parser Options
clusterShapesOpts = GenerateClusters <$> (ClusterShapesOpts <$> annotationsFilesOpt <*> segmenterOpt <*> contiguityThresholdOpt)

clusterShapes :: Options -> ClusterShapesOpts -> IO ()
clusterShapes optns o = do
  csvFiles   <- sequence $ map globExpHome (clusterCSVFilesPatterns o)

  allShapes <- mapM (collectShapesFromPage (lookup (clusterSegmenter o) segmenters)) (concat csvFiles)
  let clusters = cluster (concat allShapes)

  putStrLn $ Map.showTreeWith showCluster True True clusters

  where
    collectShapesFromPage Nothing _       = fail "Unkown segmenter"
    collectShapesFromPage (Just shapesIn) p = do
      page <- loadPage p
      return $ shapesIn page optns

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
  (DumpShapes os)       -> dumpShapeFiles o os
  (OverlayShapes os)    -> overlayShapes o os
  (GenerateClusters os) -> clusterShapes o os
  _                     -> fail "Unrecognised command"

main :: IO ()
main = execParser opts >>= dispatchCommand

