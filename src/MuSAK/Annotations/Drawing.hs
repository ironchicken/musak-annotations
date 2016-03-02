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

module MuSAK.Annotations.Drawing where

import           Control.Monad (foldM, unless)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.GD as G
import           MuSAK.Annotations.Geometry hiding (sizeOf)
import qualified MuSAK.Annotations.Geometry as Geom (sizeOf)
import           MuSAK.Annotations.Segmentation (Segmenter)
import           MuSAK.Annotations.Types
import           System.Directory (doesFileExist)
import           Text.Printf (printf)

sizeOf :: Shape -> G.Size
sizeOf = Geom.sizeOf

lineColour :: G.Color
lineColour = (G.rgb 0 0 255)

drawMark :: G.Image -> Maybe Point -> Mark -> IO ()
drawMark img (Just off) m = G.drawLine (translateToO (start m) off) (translateToO (end m) off) lineColour img
drawMark img Nothing    m = G.drawLine (start m) (end m) lineColour img

drawShape :: G.Image -> Shape -> Maybe Point ->  IO ()
drawShape img s offs = mapM_ (drawMark img offs) (sh_marks s)

makeShapeImg :: Shape -> IO G.Image
makeShapeImg s = do
  img <- G.newImage (sizeOf s)
  G.fillImage (G.rgba 0 0 0 127) img
  drawShape img s (Just (offset s))
  return img

drawRect :: G.Image -> (Int, Int, Int, Int) -> IO ()
drawRect img (left, top, right, bottom) = do
  G.drawLine (left,top) (right,top) lineColour img
  G.drawLine (right,top) (right,bottom) lineColour img
  G.drawLine (right,bottom) (left,bottom) lineColour img
  G.drawLine (left,bottom) (left,top) lineColour img

drawBorder :: G.Image -> Shape -> Maybe Point -> IO ()
drawBorder img s (Just (x, y)) = do
  let (left, top, right, bottom) = bounds s
  drawRect img (left+x, top+y, right+x, bottom+y)

drawBorder img s Nothing = do
  let (left, top, right, bottom) = bounds s
  drawRect img (left, top, right, bottom)

makePageImgWithShapes :: Segmenter -> FilePath -> Page -> Options -> IO G.Image
makePageImgWithShapes shapesIn scorePage p opts = do
  e <- doesFileExist scorePage
  unless e $ fail $ scorePage ++ " does not exist."
  img <- G.loadJpegFile scorePage
  mapM_ (\s -> do { drawShape img s (Just (0,(-220))); drawBorder img s (Just (0,220)) }) (shapesIn p opts)
  return img

clusterImg :: [(Shape, Shape, Double)] -> IO G.Image
clusterImg [] = G.newImage (1,1)
clusterImg ds = do
  img      <- G.newImage clusterImageSize
  drawShapeWithDist (img, 0) (nullShape, (first . head) ds, 0.0)
  (img, _) <- foldM drawShapeWithDist (img, max minWidth (fst $ sizeOf $ (first . head) ds)) ds
  return img

  where
    drawShapeWithDist (img, xOffs) (_, shape, distance) = do
      let offs   = Just (x - xOffs, y)
          (x, y) = (offset shape)
      drawShape img shape offs
      drawLabel img shape distance xOffs
      return (img, xOffs + max minWidth (fst $ sizeOf shape))

    drawLabel img shape distance offs = do
      _ <- G.drawString font 9 0 (offs + 5, yOffs) (sh_long_label shape) labelColour img
      G.drawString font 9 0 (offs + 5, yOffs + yOffs) (printf "%0.4f" (distance :: Double)) labelColour img
      where
        -- FIXME Make this a command line option
        font        = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
        labelColour = (G.rgb 0 255 0)
        yOffs       = 15

    shapes           = (first . head) ds : map second ds
    clusterImageSize = (sum $ map (\dim -> max minWidth (fst dim)) imageSizes, maximum $ (map snd) imageSizes)
    imageSizes       = map sizeOf shapes
    minWidth         = 130

    first a  = s where (s, _, _) = a
    second a = s where (_, s, _) = a

drawClusters :: Map String [(Shape, Shape, Double)] -> IO ()
drawClusters m = sequence_ $ Map.mapWithKey saveClusterImg m
  where
    saveClusterImg k c = do
      img <- clusterImg $ sortBy (\(_,_,a) (_,_,b) -> compare a b) c
      G.savePngFile (k ++ "_cluster.png") img
