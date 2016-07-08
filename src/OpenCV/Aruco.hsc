{-# language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Aruco where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Traversable (for)
import qualified "vector" Data.Vector as V
import "base" Data.Word
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.Internal
import "this" OpenCV.TypeLevel
import "base" System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/aruco.hpp"
C.include "opencv2/aruco/charuco.hpp"
C.include "opencv2/core.hpp"
C.include "iostream"

C.include "aruco.hpp"

C.using "namespace cv"

#include <bindings.dsl.h>

#include "opencv2/aruco.hpp"
#include "opencv2/aruco/charuco.hpp"
#include "opencv2/aruco/dictionary.hpp"

#include "aruco.hpp"

--------------------------------------------------------------------------------

data PredefinedDictionaryName
  = DICT_4X4_50
  | DICT_4X4_100
  | DICT_4X4_250
  | DICT_4X4_1000
  | DICT_5X5_50
  | DICT_5X5_100
  | DICT_5X5_250
  | DICT_5X5_1000
  | DICT_6X6_50
  | DICT_6X6_100
  | DICT_6X6_250
  | DICT_6X6_1000
  | DICT_7X7_50
  | DICT_7X7_100
  | DICT_7X7_250
  | DICT_7X7_1000
  | DICT_ARUCO_ORIGINAL

#num DICT_4X4_50
#num DICT_4X4_100
#num DICT_4X4_250
#num DICT_4X4_1000
#num DICT_5X5_50
#num DICT_5X5_100
#num DICT_5X5_250
#num DICT_5X5_1000
#num DICT_6X6_50
#num DICT_6X6_100
#num DICT_6X6_250
#num DICT_6X6_1000
#num DICT_7X7_50
#num DICT_7X7_100
#num DICT_7X7_250
#num DICT_7X7_1000
#num DICT_ARUCO_ORIGINAL

unmarshalDictionaryName :: PredefinedDictionaryName -> Int32
unmarshalDictionaryName n =
  case n of
    DICT_4X4_50 -> c'DICT_4X4_50
    DICT_4X4_100 -> c'DICT_4X4_100
    DICT_4X4_250 -> c'DICT_4X4_250
    DICT_4X4_1000 -> c'DICT_4X4_1000
    DICT_5X5_50 -> c'DICT_5X5_50
    DICT_5X5_100 -> c'DICT_5X5_100
    DICT_5X5_250 -> c'DICT_5X5_250
    DICT_5X5_1000 -> c'DICT_5X5_1000
    DICT_6X6_50 -> c'DICT_6X6_50
    DICT_6X6_100 -> c'DICT_6X6_100
    DICT_6X6_250 -> c'DICT_6X6_250
    DICT_6X6_1000 -> c'DICT_6X6_1000
    DICT_7X7_50 -> c'DICT_7X7_50
    DICT_7X7_100 -> c'DICT_7X7_100
    DICT_7X7_250 -> c'DICT_7X7_250
    DICT_7X7_1000 -> c'DICT_7X7_1000
    DICT_ARUCO_ORIGINAL -> c'DICT_ARUCO_ORIGINAL

newtype Dictionary =
  Dictionary {unDictionary :: ForeignPtr C'Ptr_ArucoDictionary}

type instance C Dictionary = C'Ptr_ArucoDictionary

instance WithPtr Dictionary where
  withPtr = withForeignPtr . unDictionary

instance FromPtr Dictionary where
  fromPtr = objFromPtr Dictionary $ \ptr ->
    [CU.block| void {
      Ptr<aruco::Dictionary> * dictionaryPtrPtr =
        $(Ptr_ArucoDictionary * ptr);
      dictionaryPtrPtr->release();
      delete dictionaryPtrPtr;
    }|]

predefinedDictionary
  :: PredefinedDictionaryName -> Dictionary
predefinedDictionary d = unsafePerformIO $ fromPtr
    [CU.block|Ptr_ArucoDictionary * {
      return new Ptr<aruco::Dictionary>(
        aruco::getPredefinedDictionary($(int32_t c'dictionaryName))
      );
    }|]
  where
    c'dictionaryName = unmarshalDictionaryName d

--------------------------------------------------------------------------------

newtype CharucoBoard =
  CharucoBoard {unCharucoBoard :: ForeignPtr C'Ptr_CharucoBoard}

type instance C CharucoBoard = C'Ptr_CharucoBoard

instance WithPtr CharucoBoard where
  withPtr = withForeignPtr . unCharucoBoard

instance FromPtr CharucoBoard where
  fromPtr = objFromPtr CharucoBoard $ \ptr ->
    [CU.block| void {
      Ptr<aruco::CharucoBoard> * dictionaryPtrPtr =
        $(Ptr_CharucoBoard * ptr);
      dictionaryPtrPtr->release();
      delete dictionaryPtrPtr;
    }|]

createCharucoBoard :: Int
                   -> Int
                   -> Float
                   -> Float
                   -> Dictionary
                   -> CharucoBoard
createCharucoBoard squaresX squaresY squareLength markerLength dictionary = unsafePerformIO $
  withPtr dictionary $ \dictionaryPtr -> fromPtr
  [CU.block|Ptr_CharucoBoard * {
    Ptr<aruco::CharucoBoard> board = aruco::CharucoBoard::create(
      $(int32_t c'squaresX), $(int32_t c'squaresY), $(float c'squareLength),
      $(float c'markerLength), *$(Ptr_ArucoDictionary * dictionaryPtr)
    );
    return new Ptr<aruco::CharucoBoard>(board);
  }|]
  where
    c'squaresX = fromIntegral squaresX
    c'squaresY = fromIntegral squaresY
    c'squareLength = realToFrac squareLength
    c'markerLength = realToFrac markerLength

{-|

This function return the image of a planar board, ready to be printed. It assumes
the Board layout specified is planar by ignoring the z coordinates of the object
points.

Example:

@
charucoBoard :: Mat ('S ['S 512, 'S 512]) ('S 1) ('S Word8)
charucoBoard =
  exceptError $
  drawCharucoBoard (createCharucoBoard 8 8 1 0.5 (predefinedDictionary DICT_5X5_100))
                   (V2 512 512 :: V2 Int32)
                   4 2
@

<<doc/generated/examples/charucoBoard.png charucoBoard>>

-}
drawCharucoBoard
  :: ToSize2i size2i
  => CharucoBoard
  -> size2i
  -> Int
  -> Int
  -> CvExcept (Mat ('S [w, h]) ('S 1) ('S Word8))
drawCharucoBoard board size marginSize borderBits = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr board $ \boardPtr ->
      withPtr dst $ \dstPtr ->
      withPtr (toSize2i size) $ \sizePtr ->
        [cvExcept|
          (*$(Ptr_CharucoBoard * boardPtr))->draw(
            *$(Size2i * sizePtr),
            *$(Mat * dstPtr),
            $(int32_t c'marginSize),
            $(int32_t c'borderBits)
          );
        |]
  where
    c'marginSize = fromIntegral marginSize
    c'borderBits = fromIntegral borderBits

data MarkerDetectionResult =
  MarkerDetectionResult {detectedCorners :: V.Vector (V.Vector Point2f)
                        ,detectedIds :: V.Vector Int32}

detectMarkers
  :: Mat ('S [w, h]) channels depth
  -> Dictionary
  -> MarkerDetectionResult
detectMarkers image dictionary = unsafePerformIO $
  withPtr image $ \srcPtr ->
  withPtr dictionary $ \dictPtr ->
  alloca $ \markerCornerPointsPtrPtr ->
  alloca $ \markerCornerPointsLengthsPtrPtr ->
  alloca $ \markerCornerLengthsPtr ->
  alloca $ \idsPtrPtr ->
  alloca $ \idsLengthPtr -> mask_ $ do
    [CU.block| void {
      std::vector<std::vector<cv::Point2f>> corners;
      std::vector<int> ids;

      cv::aruco::detectMarkers(
        *$(Mat * srcPtr),
        *$(Ptr_ArucoDictionary * dictPtr),
        corners,
        ids
      );

      int * idsArray = new int[ids.size()];
      int * * idsPtr = $(int32_t * * idsPtrPtr);
      *idsPtr = idsArray;
      *$(int32_t * idsLengthPtr) = ids.size();

      for(std::vector<int>::size_type i = 0; i != ids.size(); i++) {
        idsArray[i] = ids[i];
      }

      cv::Point2f * * * * markerCornerPointsPtrPtr = $(Point2f * * * * markerCornerPointsPtrPtr);
      cv::Point2f * * * markerCornerPoints = new cv::Point2f * * [corners.size()];
      *markerCornerPointsPtrPtr = markerCornerPoints;

      int * * markerCornerLengthsPtrPtr = $(int32_t * * markerCornerPointsLengthsPtrPtr);
      int * markerCornerLengths = new int[corners.size()];
      *markerCornerLengthsPtrPtr = markerCornerLengths;
      *$(int32_t * markerCornerLengthsPtr) = corners.size();

      for(std::vector<std::vector<cv::Point2f>>::size_type i = 0; i != corners.size(); i++) {
        std::vector<cv::Point2f> & markerPoints = corners[i];
        markerCornerPoints[i] = new cv::Point2f * [markerPoints.size()];
        markerCornerLengths[i] = markerPoints.size();
        for(std::vector<cv::Point2f>::size_type j = 0; j != markerPoints.size(); j++) {
          cv::Point2f & corner = markerPoints[j];
          markerCornerPoints[i][j] = new cv::Point2f(corner.x, corner.y);
        }
      }
    }|]

    idsLength <- peek idsLengthPtr
    ids <- peek idsPtrPtr >>= peekArray (fromIntegral idsLength)

    -- The number of markers found
    numMarkers <- peek markerCornerLengthsPtr

    -- The number of corner points found for each marker
    markerCornerPointsLengths <-
      peek markerCornerPointsLengthsPtrPtr >>=
      peekArray (fromIntegral numMarkers)

    -- A list of Ptr (Ptr C'Point2f) - the unmarshalled corner points arrays
    unmarshalledMarkerCornerPoints <-
      peek markerCornerPointsPtrPtr >>=
      peekArray (fromIntegral numMarkers)

    markerCornerPoints <-
      for (zip unmarshalledMarkerCornerPoints markerCornerPointsLengths) $ \(markerCornerPointsPtr,n) ->
      fmap V.fromList
           (peekArray (fromIntegral n)
                      markerCornerPointsPtr >>=
            mapM (fromPtr . pure))

    _ <- [CU.block| void {
      cv::Point2f * * * markerCornerPoints = *$(Point2f * * * * markerCornerPointsPtrPtr);
      int * markerCornerLengths = *$(int32_t * * markerCornerPointsLengthsPtrPtr);

      for (int i = 0; i < *$(int32_t * markerCornerLengthsPtr); i++) {
        delete[] markerCornerPoints[i];
      }
      delete[] *$(Point2f * * * * markerCornerPointsPtrPtr);
      delete[] *$(int32_t * * markerCornerPointsLengthsPtrPtr);
      delete[] *$(int32_t * * idsPtrPtr);
    }|]

    return MarkerDetectionResult {detectedCorners = V.fromList markerCornerPoints
                                 ,detectedIds = V.fromList ids}
