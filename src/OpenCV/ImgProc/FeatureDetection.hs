{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.FeatureDetection
    ( canny
    , houghCircles
    , Circle(..)
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Maybe
import qualified "vector" Data.Vector as V
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear ( V2(..), V3(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.TypeLevel
import "base" System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Feature Detection
--------------------------------------------------------------------------------

{- |

Finds edges in an image using the
<http://docs.opencv.org/2.4/modules/imgproc/doc/feature_detection.html#canny86 Canny86>
algorithm.

Example:

@
cannyImg
    :: forall shape channels depth
     . (Mat shape channels depth ~ Lambda)
    => Mat shape ('S 1) depth
cannyImg = exceptError $
  canny 30 200 Nothing Nothing lambda
@

<<doc/generated/examples/cannyImg.png cannyImg>>

-}
canny
    :: Double
       -- ^ First threshold for the hysteresis procedure.
    -> Double
       -- ^ Second threshold for the hysteresis procedure.
    -> Maybe Int32
       -- ^ Aperture size for the @Sobel()@ operator. If not specified defaults
       -- to @3@.
    -> Maybe Bool
       -- ^ A flag, indicating whether a more accurate L2 norm should be used.
       -- If 'False' or 'Nothing' the default L1 norm will be used.
    -> Mat ('S [w, h]) ('S 1) ('S Word8)
       -- ^ Single-channel 8-bit input image.
    -> CvExcept (Mat ('S [h, w]) ('S 1) ('S Word8))
canny threshold1 threshold2 apertureSize l2gradient src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
          [cvExcept|
            cv::Canny
            ( *$(Mat * srcPtr)
            , *$(Mat * dstPtr)
            , $(double c'threshold1)
            , $(double c'threshold2)
            , $(int32_t c'apertureSize)
            , $(bool c'l2Gradient)
            );
          |]
  where
    c'threshold1 = realToFrac threshold1
    c'threshold2 = realToFrac threshold2
    c'apertureSize = fromMaybe 3 apertureSize
    c'l2Gradient = fromBool (fromMaybe False l2gradient)

data Circle =
  Circle {circleCenter :: V2 Float
         ,circleRadius :: Float}
  deriving (Show)

{- |

Finds circles in a grayscale image using a modification of the Hough
transformation.

Example:

@
houghCircleTraces
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Circles_1000x625)
    => Mat (ShapeT [height, width]) ('S channels) ('S depth)
houghCircleTraces = exceptError $ do
  imgG <- cvtColor bgr gray circles_1000x625
  let circles = houghCircles 1 10 Nothing Nothing Nothing Nothing imgG
  withMatM (Proxy :: Proxy [height, width])
           (Proxy :: Proxy channels)
           (Proxy :: Proxy depth)
           white $ \imgM -> do
    void $ matCopyToM imgM (V2 0 0) circles_1000x625 Nothing
    forM_ circles $ \c -> do
      circle imgM (round \<$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
@

<<doc/generated/examples/houghCircleTraces.png houghCircleTraces>>

-}

houghCircles
  :: Double
     -- ^ Inverse ratio of the accumulator resolution to the image resolution.
     -- For example, if @dp=1@, the accumulator has the same resolution as the
     -- input image. If @dp=2@, the accumulator has half as big width and height.
  -> Double
     -- ^ Minimum distance between the centers of the detected circles. If the
     -- parameter is too small, multiple neighbor circles may be falsely
     -- detected in addition to a true one. If it is too large, some circles may
     -- be missed.
  -> Maybe Double
     -- ^ The higher threshold of the two passed to the 'canny' edge detector
     -- (the lower one is twice smaller). Default is 100.
  -> Maybe Double
     -- ^ The accumulator threshold for the circle centers at the detection
     -- stage. The smaller it is, the more false circles may be detected.
     -- Circles, corresponding to the larger accumulator values, will be returned
     -- first. Default is 100.
  -> Maybe Int32
     -- ^ Minimum circle radius.
  -> Maybe Int32
     -- ^ Maximum circle radius.
  -> Mat ('S [w,h]) ('S 1) ('S Word8)
  -> V.Vector Circle
houghCircles dp minDist param1 param2 minRadius maxRadius src = unsafePerformIO $
  withPtr src $ \srcPtr ->
  alloca $ \(circleLengthsPtr :: Ptr Int32) ->
  alloca $ \(circlesPtrPtr :: Ptr (Ptr (Ptr C'Vec3f))) -> mask_ $ do
    _ <- [cvExcept|
      std::vector<cv::Vec3f> circles;
      cv::HoughCircles(
        *$(Mat * srcPtr),
        circles,
        CV_HOUGH_GRADIENT,
        $(double c'dp),
        $(double c'minDist),
        $(double c'param1),
        $(double c'param2),
        $(int32_t c'minRadius),
        $(int32_t c'maxRadius)
      );

      cv::Vec3f * * * circlesPtrPtr = $(Vec3f * * * circlesPtrPtr);
      cv::Vec3f * * circlesPtr = new cv::Vec3f * [circles.size()];
      *circlesPtrPtr = circlesPtr;

      *$(int32_t * circleLengthsPtr) = circles.size();

      for (std::vector<cv::Vec3f>::size_type i = 0; i != circles.size(); i++) {
        circlesPtr[i] = new cv::Vec3f( circles[i] );
      }
    |]
    numCircles <- fromIntegral <$> peek circleLengthsPtr
    circlesPtr <- peek circlesPtrPtr
    (circles :: [V3 Float]) <-
        peekArray numCircles circlesPtr >>=
        mapM (fmap (fmap fromCFloat . fromVec3f) . fromPtr . pure)
    pure (V.fromList (map (\(V3 x y r) -> Circle (V2 x y) r) circles))
  where c'dp = realToFrac dp
        c'minDist = realToFrac minDist
        c'param1 = realToFrac (fromMaybe 100 param1)
        c'param2 = realToFrac (fromMaybe 100 param2)
        c'minRadius = fromIntegral (fromMaybe 0 minRadius)
        c'maxRadius = fromIntegral (fromMaybe 0 maxRadius)
        fromCFloat :: C.CFloat -> Float
        fromCFloat = realToFrac
