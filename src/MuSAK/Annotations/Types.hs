module MuSAK.Annotations.Types (Mark(..)) where

import Control.Applicative
import Control.Monad
import Data.Csv
import Data.Time.Format (parseTime, formatTime)
import System.Locale (TimeLocale, defaultTimeLocale, timeFmt)
import Data.Time.LocalTime
import Data.Vector as DV
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

data Mark = Mark { color  :: !ByteString
                 , pen    :: !Int
                 , startX :: !Int
                 , startY :: !Int
                 , endX   :: !Int
                 , endY   :: !Int
                 , time   :: !TimeOfDay } deriving (Show)

instance FromRecord Mark where
  parseRecord v
    | DV.length v == 7 = Mark
                           <$> v .! 0
                           <*> v .! 1
                           <*> v .! 2
                           <*> v .! 3
                           <*> v .! 4
                           <*> v .! 5
                           <*> v .! 6
    | otherwise = mzero

instance ToRecord Mark where
  toRecord (Mark c p sX sY eX eY t) = record [ toField c
                                             , toField p
                                             , toField sX
                                             , toField sY
                                             , toField eX
                                             , toField eY
                                             , toField t ]

locale :: TimeLocale
locale = defaultTimeLocale { timeFmt = "%H:%M:%S%Q" }

format :: String
format = "%H:%M:%S%Q"

parseTimeOfDay :: ByteString -> Parser TimeOfDay
parseTimeOfDay s = case parseTime locale format (BC.unpack s) of
                    Just t  -> pure t
                    Nothing -> pure $ TimeOfDay { todHour = 0, todMin = 0, todSec = 0 }

instance FromField TimeOfDay where
  parseField s = parseTimeOfDay s

instance ToField TimeOfDay where
  toField s = BC.pack (formatTime locale format s)
