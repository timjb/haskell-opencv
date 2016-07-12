{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Video
    ( -- * Motion Analysis and Object Tracking
      estimateRigidTransform
    , VideoCapture
    , openVideoCapture
    , retrieveVideoFrame
    ) where

import "transformers" Control.Monad.Trans.Except
import "base" Data.Int
import qualified "vector" Data.Vector as V
import "base" Data.Word
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.Internal
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/video.hpp"
C.include "opencv2/videoio.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/video.hpp"
#include "opencv2/videoio.hpp"

--------------------------------------------------------------------------------

-- | Computes an optimal affine transformation between two 2D point sets
--
-- <http://docs.opencv.org/3.0-last-rst/modules/video/doc/motion_analysis_and_object_tracking.html#estimaterigidtransform OpenCV Sphinx doc>
estimateRigidTransform
    :: ( ToPoint2i  srcPoint2i
       , ToPoint2i  dstPoint2i
       )
    => V.Vector srcPoint2i -- ^ Source
    -> V.Vector dstPoint2i -- ^ Destination
    -> Bool -- ^ Full affine
    -> CvExcept (Maybe (Mat (ShapeT [2, 3]) ('S 1) ('S Double)))
estimateRigidTransform src dst fullAffine = do
    result <- c'estimateRigidTransform
    -- If the c++ function can't estimate a rigid transform it will
    -- return an empty matrix. We check for this case by trying to
    -- coerce the result to the desired type.
    catchE (Just <$> coerceMat result)
           (\case CoerceMatError _msgs -> pure Nothing
                  otherError -> throwE otherError
           )
  where
    c'estimateRigidTransform = unsafeWrapException $ do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
        withArrayPtr (V.map toPoint2i src) $ \srcPtr ->
        withArrayPtr (V.map toPoint2i dst) $ \dstPtr ->
        withPtr matOut $ \matOutPtr ->
          [cvExcept|
            Mat * matOutPtr = $(Mat * matOutPtr);
            *matOutPtr =
               cv::estimateRigidTransform
               ( cv::_InputArray($(Point2i * srcPtr), $(int32_t c'srcLen))
               , cv::_InputArray($(Point2i * dstPtr), $(int32_t c'dstLen))
               , $(bool c'fullAffine)
               );
          |]

    c'srcLen     = fromIntegral $ V.length src
    c'dstLen     = fromIntegral $ V.length dst
    c'fullAffine = fromBool fullAffine

newtype VideoCapture =
  VideoCapture {unVideoCapture :: ForeignPtr C'VideoCapture}

type instance C VideoCapture = C'VideoCapture

instance WithPtr VideoCapture where
  withPtr = withForeignPtr . unVideoCapture

instance FromPtr VideoCapture where
  fromPtr = objFromPtr VideoCapture $ \ptr ->
    [CU.block| void {
      cv::VideoCapture * dictionaryPtrPtr =
        $(VideoCapture * ptr);
      dictionaryPtrPtr->release();
      delete dictionaryPtrPtr;
    }|]

openVideoCapture :: Int32 -> IO VideoCapture
openVideoCapture camId = fromPtr
  [C.block| VideoCapture * { return new cv::VideoCapture($(int32_t camId)); } |]

retrieveVideoFrame :: VideoCapture -> IO (Mat ('S ['D, 'D]) 'D ('S Word8))
retrieveVideoFrame video = do
  dst <- newEmptyMat
  withPtr video $ \v ->
    withPtr dst $ \dstPtr ->
      [C.block| void {
        *$(VideoCapture * v) >> *$(Mat * dstPtr);
      }|]
  return (unsafeCoerceMat dst)
