module Main where

import MuSAK.Annotations.Types
import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 as LBC
import Data.ByteString.Char8 as BC
import Data.Csv
import Data.Time.LocalTime
import Data.Vector as DV

marks :: [Mark]
marks = [Mark { color = (BC.pack "#00AA20"), pen = 2, startX = 6, startY = 7, endX = 120, endY = 156, time = TimeOfDay { todHour = 13, todMin = 12, todSec = 56.128 } }]

marksCSV :: LB.ByteString
marksCSV = LBC.pack "#00AA20,2,6,7,120,156,13:12:56.128\r\n"

main :: IO ()
main = do
  let m = (decode NoHeader marksCSV) :: Either String (DV.Vector Mark)
  Prelude.putStrLn $ show (encode marks)
  Prelude.putStrLn $ show m
