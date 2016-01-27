module MuSAK.Annotations.Drawing where

import           Control.Monad (unless)
import qualified Graphics.GD as G
import           MuSAK.Annotations.Geometry
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types
import           System.Directory (doesFileExist)

sizeOf :: Shape -> G.Size
sizeOf s = (max 1 (right - left), max 1 (bottom - top))
  where
    (left, top, right, bottom) = bounds s

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

makePageImgWithShapes :: FilePath -> Page -> IO G.Image
makePageImgWithShapes scorePage p = do
  e <- doesFileExist scorePage
  unless e $ fail $ scorePage ++ " does not exist."
  img <- G.loadJpegFile scorePage
  mapM_ (\s -> do { drawShape img s (Just (0,(-220))); drawBorder img s (Just (0,220)) }) (shapes p)
  return img
