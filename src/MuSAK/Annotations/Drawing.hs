module MuSAK.Annotations.Drawing where

import           Data.List (minimum, maximum)
import qualified Graphics.GD as G
import           MuSAK.Annotations.Segmentation
import           MuSAK.Annotations.Types

bounds :: Shape -> (Int, Int, Int, Int)
bounds s = (left, top, right, bottom)
  where
    xs = concat $ map (\m -> [fst (start m), fst (end m)]) (sh_marks s)
    ys = concat $ map (\m -> [snd (start m), snd (end m)]) (sh_marks s)
    left   = minimum xs
    right  = maximum xs
    top    = minimum ys
    bottom = maximum ys

offset :: Shape -> Point
offset s = (left, top)
  where
    (left, top, _, _) = bounds s

sizeOf :: Shape -> G.Size
sizeOf s = (max 1 (right - left), max 1 (bottom - top))
  where
    (left, top, right, bottom) = bounds s

translateToO :: Point -> Point -> Point
translateToO (x1,y1) (offsX,offsY) = (x1 - offsX, y1 - offsY)

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
