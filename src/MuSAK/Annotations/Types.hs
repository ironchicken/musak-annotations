module MuSAK.Annotations.Types ( Mark(..)
                               , Page(..)
                               , Point
                               , Shape(..)
                               , Weight
                               , connectionThreshold
                               , end
                               , start
                               , weight ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Csv
import           Data.Time.Format (TimeLocale, defaultTimeLocale, timeFmt, parseTime, formatTime)
import           Data.Time.LocalTime
import qualified Data.Vector as DV
import qualified Data.ByteString.Char8 as BC

type Point = (Int, Int)
type Weight = Double

data Mark = Mark { color  :: !ByteString
                 , pen    :: !Int
                 , startX :: !Int
                 , startY :: !Int
                 , endX   :: !Int
                 , endY   :: !Int
                 , time   :: !TimeOfDay }

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

instance Show Mark where
  show m = (show (start m)) ++ " -> " ++ (show (end m))

start :: Mark -> Point
start (Mark { startX = x, startY = y }) = (x, y)

end :: Mark -> Point
end (Mark { endX = x, endY = y }) = (x, y)

connectionThreshold :: Weight
connectionThreshold = 1.0

-- FIXME Should we retain the (end a) == (start b) check? It might
-- turn out that this check fails in the real data.
weight :: Mark -> Mark -> Weight
-- weight a b | end a == start b = asSeconds (time b) - asSeconds (time a)
--            | otherwise        = connectionThreshold
weight a b = asTime b - asTime a
  where asTime = asSeconds . time

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

asSeconds :: TimeOfDay -> Double
asSeconds (TimeOfDay { todHour = h, todMin = m, todSec = s }) = hs + ms + (realToFrac s)
  where
    hs = (realToFrac h) / 60.0 / 60.0
    ms = (realToFrac m) / 60.0

data Page = Page { pg_marks :: [Mark]
                 , pg_sourceFile :: FilePath } deriving (Show)

instance Eq Page where
  (==) (Page { pg_sourceFile = a }) (Page { pg_sourceFile = b }) = a == b

data Shape = Shape { sh_marks :: [Mark]
                   , sh_label :: String }

instance Show Shape where
  show (Shape { sh_marks = ms, sh_label = l }) = "{" ++ l ++ ": " ++ (show $ reverse ms) ++ "}\n"

instance Eq Shape where
  (==) (Shape { sh_marks = a }) (Shape { sh_marks = b }) = undefined
