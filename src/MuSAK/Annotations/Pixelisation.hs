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

module MuSAK.Annotations.Pixelisation ( pixels ) where

import           Codec.Picture (Pixel8)
import           Codec.Picture.Types (Image, Image(imageData), PixelBaseComponent, pixelAt)
import qualified Data.Vector.Storable as DV
import           Data.Word (Word8(..))
import qualified Graphics.Rasterific as R
import           Graphics.Rasterific.Texture (uniformTexture)
import           MuSAK.Annotations.Geometry
import           MuSAK.Annotations.Types
import           System.IO.Unsafe (unsafePerformIO)

pixels :: Shape -> DV.Vector (PixelBaseComponent Word8)
pixels s = imageData $ render (w, h) (drawShape s (Just (offset s)))
  where
    (w, h) = sizeOf s

drawShape :: Shape -> Maybe Point -> R.Drawing px ()
drawShape s offs =
  R.stroke 4 R.JoinRound (R.CapRound, R.CapRound) $
    R.polyline $ concat $ map (markToVec offs) (sh_marks s)

render :: (Int, Int) -> R.Drawing Pixel8 () -> Image (PixelBaseComponent Word8)
render (w, h) d = R.renderDrawing w h white . R.withTexture (uniformTexture drawColor) $ d
  where
    white     = 255 :: Pixel8
    drawColor = 0 :: Pixel8

pointToVec :: Point -> R.Point
pointToVec (x, y) = R.V2 (fromIntegral x) (fromIntegral y)

markToVec :: Maybe Point -> Mark -> [R.Point]
markToVec (Just off) m = [ pointToVec (translateToO (start m) off)
                         , pointToVec (translateToO (end m) off) ]
markToVec Nothing m    = [ pointToVec (start m)
                         , pointToVec (end m) ]
