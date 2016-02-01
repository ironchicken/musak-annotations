module MuSAKTests (tests) where

import Distribution.TestSuite

import Data.ByteString.Lazy as LB hiding (head)
import Data.ByteString.Lazy.Char8 as LBC hiding (head)
import Data.ByteString.Char8 as BC hiding (head)
import Data.Csv
import Data.Time.LocalTime
import Data.Vector as DV hiding ((++), head)
import MuSAK.Annotations.Geometry
import MuSAK.Annotations.Similarity.Turning as ST
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
    sh_label = "square10x10"
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

testMarkLenInt = TestInstance {
  run = return $ Finished $ if markLenInt (head (sh_marks square10x10)) == 10
                            then Pass
                            else Fail $ "Length of first mark in 10x10 square wrong: " ++ (show $ markLenInt (head (sh_marks square10x10)))
  , name = "10x10 square first mark has length 10"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMarkLenInt
  }

testTurningDistanceEq :: TestInstance
testTurningDistanceEq = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepSquare10x10 tRepSquare10x10) sq10x10Dist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between equal shapes is wrong: " ++ (show $ ST.distance tRepSquare10x10 tRepSquare10x10)
  , name = "10x10 square self distance (by turning function measure) is 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceEq
  }
  where tRepSquare10x10 = ST.turningRep square10x10
        sq10x10Dist     = (square10x10, square10x10, 0.0)

square20x20 :: Shape
square20x20 = Shape {
    sh_label = "square20x20"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 30,
                        endX = 10, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 10,
                        endX = 30, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.1 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 30, startY = 10,
                        endX = 30, endY = 30,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 30, startY = 30,
                        endX = 10, endY = 30,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.3 } }
               ] }

testTurningDistanceTrans :: TestInstance
testTurningDistanceTrans = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepSquare10x10 tRepSquare20x20) sq20x20Dist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between translated shapes is wrong: " ++ (show $ ST.distance tRepSquare10x10 tRepSquare20x20)
  , name = "Distance (by turning function measure) between 10x10 and 20x20 squares is 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceTrans
  }
  where tRepSquare10x10 = ST.turningRep square10x10
        tRepSquare20x20 = ST.turningRep square20x20
        sq20x20Dist     = (square10x10, square20x20, 0.0)

triangle10x10 :: Shape
triangle10x10 = Shape {
    sh_label = "triangle10x10"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 10,
                        endX = 10, endY = 20,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 20,
                        endX = 20, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.1 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 20, startY = 10,
                        endX = 10, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               ] }

triangle20x20 :: Shape
triangle20x20 = Shape {
    sh_label = "triangle20x20"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 10,
                        endX = 10, endY = 30,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 30,
                        endX = 30, endY = 30,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.1 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 30, startY = 30,
                        endX = 10, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               ] }

testTurningDistanceGrownTriangle :: TestInstance
testTurningDistanceGrownTriangle = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepTriangle10x10 tRepTriangle20x20) triangDist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between 10x10 triangle and 20x20 triangle is wrong: " ++ (show $ ST.distance tRepTriangle10x10 tRepTriangle20x20)
  , name = "Distance (by turning function measure) between 10x10 triangle and 20x20 triangle is 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceGrownTriangle
  }
  where tRepTriangle10x10 = ST.turningRep triangle10x10
        tRepTriangle20x20 = ST.turningRep triangle20x20
        triangDist        = (triangle10x10, triangle20x20, 0.0)

triangle20x5 :: Shape
triangle20x5 = Shape {
    sh_label = "triangle20x5"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 30, startY = 10,
                        endX = 30, endY = 30,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 30, startY = 30,
                        endX = 35, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.1 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 35, startY = 10,
                        endX = 30, endY = 10,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               ] }

testTurningDistanceTransformedTriangles :: TestInstance
testTurningDistanceTransformedTriangles = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepTriangle10x10 tRepTriangle20x5) triangDist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between 10x10 triangle and 20x5 triangle is wrong: " ++ (show $ ST.distance tRepTriangle10x10 tRepTriangle20x5)
  , name = "Distance (by turning function measure) between 10x10 triangle and 20x5 triangle is 0.4722"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceTransformedTriangles
  }
  where tRepTriangle10x10 = ST.turningRep triangle10x10
        tRepTriangle20x5  = ST.turningRep triangle20x5
        triangDist        = (triangle10x10, triangle20x5, 0.4722)

tests :: IO [Test]
tests = return [ Test testMarkParse
               , Test testBounds
               , Test testMarkLenInt
               , Test testTurningDistanceEq
               , Test testTurningDistanceTrans
               , Test testTurningDistanceGrownTriangle
               , Test testTurningDistanceTransformedTriangles
               ]
