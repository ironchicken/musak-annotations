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
import qualified Data.Vector.Storable as DV
import           Data.Word (Word8(..))
import           MuSAK.Annotations.Pixelisation (pixels)
import           MuSAK.Annotations.Types

integralImage :: (Int, Int) -> DV.Vector (PixelBaseComponent Word8) -> DV.Vector (PixelBaseComponent Word8)
integralImage (w, h) img = DV.generate (w * h) (\idx -> rowSum idx + colSum idx)
  where
    rowSum i    = DV.foldr (+) 0 $ DV.slice (y * w) (x + 1) img
      where (x, y) = coordsIdx i
    colSum i    = foldr (+) 0 $ map (\i -> (DV.!) img i) (colIdxs i)
      where (x, y) = coordsIdx i
    colIdxs i   = [n * w + x | n <- [0..(y - 1)] ]
      where (x, y) = coordsIdx i
    coordsIdx i = ( i `mod` w , i `div` h )
