module MuSAKTests (tests) where

import Distribution.TestSuite

import MuSAK.Annotations.Types
import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 as LBC
import Data.ByteString.Char8 as BC
import Data.Csv
import Data.Time.LocalTime
import Data.Vector as DV

testMarkParse :: TestInstance
testMarkParse = TestInstance {
    run = return $ checkMarks decMarks encMarks
  , name = "Parse marks from CSV"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMarkParse
  }
  where
    checkMarks (Right dec) enc =
      Finished $ if DV.toList dec == marks && enc == marksCSV
                 then Pass
                 else Fail "Encoding and decoding of CSV Marks failed."
    checkMarks (Left msg) _ =
      Finished $ Fail "Failed to decode marks CSV string."
    marks = [Mark { color = (BC.pack "#00AA20"), pen = 2, startX = 6, startY = 7, endX = 120, endY = 156, time = TimeOfDay { todHour = 13, todMin = 12, todSec = 56.128 } }]
    marksCSV = LBC.pack "#00AA20,2,6,7,120,156,13:12:56.128\r\n"
    decMarks = (decode NoHeader marksCSV) :: Either String (DV.Vector Mark)
    encMarks = encode marks

tests :: IO [Test]
tests = return [Test testMarkParse
               ]
