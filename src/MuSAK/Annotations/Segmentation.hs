module MuSAK.Annotations.Segmentation where

import qualified MuSAK.Annotations.Segmentation.Contiguity as Contiguity
import           MuSAK.Annotations.Types (Page(..), Shape(..))

type Segmenter = (Page -> [Shape])

segmenters :: [(String, Segmenter)]
segmenters = [ ("contiguity", Contiguity.shapes) ]
