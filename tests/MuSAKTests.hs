module MuSAKTests (tests) where

import Distribution.TestSuite

import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 as LBC
import Data.ByteString.Char8 as BC
import Data.Csv
import Data.Time.LocalTime
import Data.Vector as DV hiding ((++))
import MuSAK.Annotations.Geometry
import MuSAK.Annotations.Types

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

square10x10 :: Shape
square10x10 = Shape {
    sh_label = "square"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 0, startY = 10,
                        endX = 0, endY = 0,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 0, startY = 0,
                        endX = 10, endY = 0,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.1 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 0,
                        endX = 10, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 10,
                        endX = 0, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.3 } }
               ] }

testBounds = TestInstance {
    run = return $ Finished $ if bounds square10x10 == (0, 0, 10, 10)
                              then Pass
                              else Fail $ "Bounds of 10x10 square wrong: " ++ (show $ bounds square10x10)
  , name = "10x10 square has bounds (0, 0, 10, 10)"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testBounds
  }

tests :: IO [Test]
tests = return [ Test testMarkParse
               , Test testBounds
               ]
