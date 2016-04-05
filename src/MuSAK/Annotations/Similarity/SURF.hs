-- musak-annotations - A tool for analysing the annotations produced
-- by the MuSAK system
--
-- Copyright (C) 2016 Richard Lewis
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

module MuSAK.Annotations.Similarity.SURF where

import           Codec.Picture.Types (PixelBaseComponent)
import           Control.Monad.ST (runST)
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Storable.Mutable as DVM
import           Data.Word (Word8(..))
import           MuSAK.Annotations.Pixelisation (pixels)
import           MuSAK.Annotations.Types

integralImage :: (Int, Int) -> DVS.Vector (PixelBaseComponent Word8) -> DVS.Vector (PixelBaseComponent Word8)
integralImage (w, h) img = DVS.create $ do
  intImg <- DVM.new (w * h)
  mapM_ (writeElem intImg) [0..(w * h) - 1]
  return intImg
  where
    writeElem ii i | y == 0    = DVS.mapM_ (\n -> DVM.write ii i n) rowSum
                   | otherwise = DVS.mapM_ (\n -> do { abv <- DVM.read ii (i - w);
                                                       DVM.write ii i (n + abv) }) rowSum
      where rowSum = DVS.scanl1 (+) $ DVS.slice (y * w) (x + 1) img
            (x, y) = ( i `mod` w , i `div` h )
