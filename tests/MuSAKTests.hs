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

module MuSAKTests (tests) where

import Distribution.TestSuite

import Codec.Picture.Types (PixelBaseComponent)
import Data.ByteString.Lazy as LB hiding (map, all, zip, head, filter, putStrLn)
import Data.ByteString.Lazy.Char8 as LBC hiding (map, all, zip, head, filter, putStrLn)
import Data.ByteString.Char8 as BC hiding (map, all, zip, head, filter, putStrLn)
import Data.Csv
import Data.Time.LocalTime
import qualified Data.Vector as DV hiding ((++), map, all, zip, head, filter, putStrLn)
import qualified Data.Vector.Storable as DVS
import Data.Word (Word8(..))
import MuSAK.Annotations.Geometry
import MuSAK.Annotations.IOUtils (loadPage)
import MuSAK.Annotations.Segmentation.Contiguity (shapes)
import MuSAK.Annotations.Similarity.SURF as SS
import MuSAK.Annotations.Similarity.Turning as ST
import MuSAK.Annotations.Types
import System.Directory (doesFileExist)

dummyOpts :: MuSAK.Annotations.Types.Options
dummyOpts = DumpShapes $ DumpShapesOpts {
    dumpCSVFilesPatterns = []
  , dumpSegmenter        = "contiguity"
  , dumpContiguityThresh = 100 }

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
  , sh_long_label = "square10x10"
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
  , sh_long_label = "square20x20"
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
  , sh_long_label = "triangle10x10"
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
  , sh_long_label = "triangle20x20"
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
  , sh_long_label = "triangle20x5"
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

rectangle10x20 :: Shape
rectangle10x20 = Shape {
    sh_label = "rectangle10x20"
  , sh_long_label = "rectangle10x20"
  , sh_marks = [ Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 0, startY = 20,
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
                        endX = 10, endY = 20,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.2 } }
               , Mark { color = (BC.pack "#00AA20"),
                        pen = 2,
                        startX = 10, startY = 20,
                        endX = 0, endY = 20,
                        time = TimeOfDay { todHour = 0,
                                           todMin = 0,
                                           todSec = 0.3 } }
               ] }

testTurningDistanceSqRect :: TestInstance
testTurningDistanceSqRect = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepSquare10x10 tRepRectangle10x20) sqRectDist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between 10x10 square and 20x10 rectangle is wrong: " ++ (show $ ST.distance tRepSquare10x10 tRepRectangle10x20)
  , name = "Distance (by turning function measure) between 10x10 square and 20x10 rectangle is 0.3962"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceSqRect
  }
  where tRepSquare10x10    = ST.turningRep square10x10
        tRepRectangle10x20 = ST.turningRep rectangle10x20
        sqRectDist         = (square10x10, rectangle10x20, 0.3962)

testTurningDistanceRectSq :: TestInstance
testTurningDistanceRectSq = TestInstance {
    run = return $ Finished $ if distEq 1e-4 (ST.distance tRepRectangle10x20 tRepSquare10x10) sqRectDist
                            then Pass
                            else Fail $ "Distance computed (by turning function measure) between 20x10 rectangle and 10x10 square is wrong: " ++ (show $ ST.distance tRepRectangle10x20 tRepSquare10x10)
  , name = "Distance (by turning function measure) between 20x10 rectangle and 10x10 square is 0.3962"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testTurningDistanceRectSq
  }
  where tRepSquare10x10    = ST.turningRep square10x10
        tRepRectangle10x20 = ST.turningRep rectangle10x20
        sqRectDist         = (rectangle10x20, square10x10, 0.3962)

musakAnnotationsPagePath :: FilePath
musakAnnotationsPagePath = undefined

runTestMuSAKShapesEq :: IO Progress
runTestMuSAKShapesEq = do
  prog <- doesFileExist musakAnnotationsPagePath >>= withPageFileExists
  return prog
  where
    withPageFileExists False = return $ Finished $ Error (musakAnnotationsPagePath ++ " does not exist.")
    withPageFileExists True = do
      page <- loadPage musakAnnotationsPagePath
      let ss = (shapes page dummyOpts)
      return $ cmpFirstShape ss

    cmpFirstShape []                            = Finished $ Error "Empty page"
    cmpFirstShape ((Shape { sh_marks = [] }):_) = Finished $ Error "Empty shape"
    cmpFirstShape (shape:_) =
      Finished $ if distEq 1e-4 (ST.distance tRepShape tRepShape) eqDist
                 then Pass
                 else Fail $ "Distance computed (by turning function measure) between equal shapes from MuSAK data-set is wrong: " ++ (show $ ST.distance tRepShape tRepShape)
      where tRepShape = ST.turningRep shape
            eqDist    = (shape, shape, 0.0)

testMuSAKShapesEq :: TestInstance
testMuSAKShapesEq = TestInstance {
    run = runTestMuSAKShapesEq
  , name = "Distance (by turning function measure) between equal shapes from MuSAK data-set is 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMuSAKShapesEq
  }

runTestMuSAKShapesSimilar :: IO Progress
runTestMuSAKShapesSimilar = do
  pg1FPE <- doesFileExist pg1FP
  pg2FPE <- doesFileExist pg2FP
  withPageFilesExists (pg1FPE && pg2FPE)

  where
    withPageFilesExists False = return $ Finished $ Error ("Some files do not exist: " ++ pg1FP ++ ", " ++ pg2FP)
    withPageFilesExists True = do
      pg1 <- loadPage pg1FP
      pg2 <- loadPage pg2FP
      let ssPg1 = filter (\s -> (sh_long_label s) == pg1ShapeLabel) (shapes pg1 dummyOpts)
          ssPg2 = filter (\s -> (sh_long_label s) == pg2ShapeLabel) (shapes pg2 dummyOpts)
      return $ cmpFirstShapes ssPg1 ssPg2

    cmpFirstShapes (sh1:_) (sh2:_) =
      Finished $ if distCmp (ST.distance tRepsh1 tRepsh2) maxDist == LT
                 then Pass
                 else Fail $ "Distance computed (by turning function measure) between " ++ pg1ShapeLabel ++ " and " ++ pg2ShapeLabel ++ " from MuSAK data-set is too large: " ++ (show $ ST.distance tRepsh1 tRepsh2)
      where
        tRepsh1 = ST.turningRep sh1
        tRepsh2 = ST.turningRep sh2
        maxDist = (sh1, sh2, maximumDistance)

    cmpFirstShapes _ _ = Finished $ Error "Empty page"

    pg1FP           = undefined
    pg1ShapeLabel   = undefined
    pg2FP           = undefined
    pg2ShapeLabel   = undefined
    maximumDistance = undefined -- 0.12

testMuSAKShapesSimilar :: TestInstance
testMuSAKShapesSimilar = TestInstance {
    run = runTestMuSAKShapesSimilar
  , name = "Distance (by turning function measure) between similar shapes from MuSAK data-set is within threshold"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMuSAKShapesEq
  }

runIntegralImageTest :: IO Progress
runIntegralImageTest =
  let
    intImgs = map (uncurry SS.integralImage) srcImgs
    srcImgs = [ ((3, 3),
                 DVS.fromList ([ 1, 1, 1
                               , 1, 1, 1
                               , 1, 1, 1 ] :: [PixelBaseComponent Word8]))
              , ((2, 2),
                 DVS.fromList ([ 1, 5
                               , 2, 4 ] :: [PixelBaseComponent Word8]))
              , ((4, 4),
                 DVS.fromList ([ 4, 1, 2, 2
                               , 0, 4, 1, 3
                               , 3, 1, 0, 4
                               , 2, 1, 3, 2 ] :: [PixelBaseComponent Word8])) ]
    expIIs  = [ DVS.fromList ([ 1, 2, 3
                              , 2, 4, 6
                              , 3, 6, 9 ] :: [PixelBaseComponent Word8])
              , DVS.fromList ([ 1, 6
                              , 3, 12 ] :: [PixelBaseComponent Word8])
              , DVS.fromList ([ 4,  5,  7,  9
                              , 4,  9, 12, 17
                              , 7, 13, 16, 25
                              , 9, 16, 22, 33 ] :: [PixelBaseComponent Word8]) ]
    in return $ Finished $ if intImgs == expIIs
                           then Pass
                           else Fail $ "Computed integral image does not match expected integral image: expected: " ++ (show expIIs) ++ "; computed: " ++ (show intImgs)

testIntegralImage :: TestInstance
testIntegralImage = TestInstance {
    run = runIntegralImageTest
  , name = "Integral image"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testIntegralImage
  }

tests :: IO [Test]
tests = return [ Test testMarkParse
               , Test testBounds
               , Test testMarkLenInt
               , Test testTurningDistanceEq
               , Test testTurningDistanceTrans
               , Test testTurningDistanceGrownTriangle
               , Test testTurningDistanceTransformedTriangles
               , Test testTurningDistanceSqRect
               , Test testTurningDistanceRectSq
               , Test testMuSAKShapesEq
               , Test testMuSAKShapesSimilar
               , Test testIntegralImage
               ]
