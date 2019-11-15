{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.HSV
-- Copyright   : [2016..2019] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Colours in the HSV (hue-saturation-value) cylindrical-coordinate
-- representation of points in the RGB colour space.
--
-- <https://en.wikipedia.org/wiki/HSL_and_HSV>
--

module Data.Array.Accelerate.Data.Colour.HSV (

  Colour,
  HSV(..),

  hsv,
  clamp,
  toRGB, fromRGB,
  hue,
  saturation,
  value,

) where

import Data.Array.Accelerate                                        as A hiding ( clamp )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product                                ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar                            ( Elt(..), Tuple(..) )

import Data.Array.Accelerate.Data.Colour.RGB                        ( RGB(..) )
import Data.Array.Accelerate.Data.Colour.Names                      as C

import Data.Functor
import Data.Typeable
import Prelude                                                      ( fromInteger )   -- ghc < 8 bug
import qualified Prelude                                            as P


-- | A HSV colour value
--
type Colour = HSV Float

-- | Construct a HSV colour value from the individual channel components. The
-- hue component is measured in degrees and wrapped to the range [0..360), while
-- the saturation and value are clamped to the range [0..1].
--
hsv :: Exp Float        -- ^ hue component
    -> Exp Float        -- ^ saturation component
    -> Exp Float        -- ^ value component
    -> Exp Colour
hsv h s v
  = clamp
  $ lift (HSV h s v)


clamp :: Exp Colour -> Exp Colour
clamp (unlift -> HSV h s v)
  = lift
  $ HSV (fmod h 360) (c s) (c v)
  where
    c x = 0 `max` x `min` 1

fmod :: Exp Float -> Exp Float -> Exp Float
fmod n d = n - f * d
  where
    f = fromIntegral (floor (n / d) :: Exp Int)

-- | Convert a HSV colour to an RGB colour-space value
--
toRGB :: Exp (HSV Float) -> Exp (RGB Float)
toRGB (unlift -> HSV h s v) = rgb
  where
    c   = v * s
    h'  = h / 60
    x   = c * (1 - abs ((h' `fmod` 2) - 1))
    --
    m   = v - c
    c'  = c + m
    x'  = x + m
    --
    rgb = h' < 1 ? ( lift (RGB c' x' m)
        , h' < 2 ? ( lift (RGB x' c' m)
        , h' < 3 ? ( lift (RGB m  c' x')
        , h' < 4 ? ( lift (RGB m  x' c')
        , h' < 5 ? ( lift (RGB x' m  c')
        ,          ( lift (RGB c' m  x') ))))))


-- | Convert a point in the RGB colour-space to a point in the HSV colour-space.
--
fromRGB :: Exp (RGB Float) -> Exp (HSV Float)
fromRGB (unlift -> RGB r g b) = lift (HSV h s v)
  where
    mx = P.maximum [r,g,b]
    mn = P.minimum [r,g,b]
    c  = mx - mn
    --
    v  = mx
    s  = c == 0 ? ( 0, c / mx  )
    h  = c == 0 ? ( 0, h0 * 60 )
    --
    h0 = mx == r ? ( ((g-b)/c) `fmod` 6
       , mx == g ? ( ((b-r)/c) + 2
       , mx == b ? ( ((r-g)/c) + 4
       , {- otherwise -} 0 )))


-- | Return the HSV-hue of an RGB colour
--
hue :: Exp (RGB Float) -> Exp Float
hue (unlift . fromRGB -> HSV h _ _) = h

-- | Return the HSV-saturation of an RGB colour
--
saturation :: Exp (RGB Float) -> Exp Float
saturation (unlift . fromRGB -> HSV _ s _) = s

-- | Return the HSV-value of an RGB colour
--
value :: Exp (RGB Float) -> Exp Float
value (unlift . fromRGB -> HSV _ _ v) = v


-- Accelerate bits
-- ---------------

-- HSV colour space
--
data HSV a = HSV a a a
  deriving (P.Show, P.Eq, Functor, Typeable, Generic)

instance Elt a => Elt (HSV a)
instance Elt a => IsProduct Elt (HSV a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (HSV a) where
  type Plain (HSV a)    = HSV (Plain a)
  lift (HSV h s v)      = Exp . Tuple $ NilTup `SnocTup` lift h `SnocTup` lift s `SnocTup` lift v

instance Elt a => Unlift Exp (HSV (Exp a)) where
  unlift c      = let h = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      s = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      v = Exp $ ZeroTupIdx `Prj` c
                  in HSV h s v

instance P.Num a => P.Num (HSV a) where
  (+) (HSV h1 s1 v1 ) (HSV h2 s2 v2)
        = HSV (h1 + h2) (s1 + s2) (v1 + v2)

  (-) (HSV h1 s1 v1) (HSV h2 s2 v2)
        = HSV (h1 - h2) (s1 - s2) (v1 - v2)

  (*) (HSV h1 s1 v1) (HSV h2 s2 v2)
        = HSV (h1 * h2) (s1 * s2) (v1 * v2)

  abs (HSV h1 s1 v1)
        = HSV (abs h1) (abs s1) (abs v1)

  signum (HSV h1 s1 v1)
        = HSV (signum h1) (signum s1) (signum v1)

  fromInteger i
        = let f = P.fromInteger i
          in  HSV f f f

instance (P.Num a, P.Fractional a) => P.Fractional (HSV a) where
  (/) (HSV h1 s1 v1) (HSV h2 s2 v2)
        = HSV (h1/h2) (s1/s2) (v1/v2)

  recip (HSV h1 s1 v1)
        = HSV (recip h1) (recip s1) (recip v1)

  fromRational r
        = let f = P.fromRational r
          in  HSV f f f


instance {-# OVERLAPS #-} A.Num a => P.Num (Exp (HSV a)) where
  (+)           = lift2 ((+) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  (-)           = lift2 ((-) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  (*)           = lift2 ((*) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  abs           = lift1 (abs :: HSV (Exp a) -> HSV (Exp a))
  signum        = lift1 (signum :: HSV (Exp a) -> HSV (Exp a))
  fromInteger i = let f = P.fromInteger i :: Exp a
                  in lift $ HSV f f f

instance {-# OVERLAPS #-} A.Fractional a => P.Fractional (Exp (HSV a)) where
  (/)            = lift2 ((/) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  recip          = lift1 (recip :: HSV (Exp a) -> HSV (Exp a))
  fromRational r = let f = P.fromRational r :: Exp a
                   in lift $ HSV f f f


-- Named colours
-- -------------

instance NamedColour (HSV Float) where
  -- Whites
  antiqueWhite      = HSV  34.3044 0.1400 0.9804
  azure             = HSV 180.0000 0.0588 1.0000
  bisque            = HSV  32.5411 0.2314 1.0000
  blanchedAlmond    = HSV  36.0122 0.1961 1.0000
  cornsilk          = HSV  47.9825 0.1373 1.0000
  eggshell          = HSV  33.0000 0.2020 0.9900
  floralWhite       = HSV  40.0000 0.0588 1.0000
  gainsboro         = HSV   0.0000 0.0000 0.8627
  ghostWhite        = HSV 240.0000 0.0275 1.0000
  honeydew          = HSV 120.0000 0.0588 1.0000
  ivory             = HSV  60.0000 0.0588 1.0000
  lavender          = HSV 240.0000 0.0800 0.9804
  lavenderBlush     = HSV 340.0000 0.0588 1.0000
  lemonChiffon      = HSV  54.0031 0.1961 1.0000
  linen             = HSV  30.0000 0.0800 0.9804
  mintCream         = HSV 150.0000 0.0392 1.0000
  mistyRose         = HSV   5.9694 0.1176 1.0000
  moccasin          = HSV  38.1048 0.2902 1.0000
  navajoWhite       = HSV  35.8582 0.3216 1.0000
  oldLace           = HSV  39.1131 0.0909 0.9922
  papayaWhip        = HSV  37.1585 0.1647 1.0000
  peachPuff         = HSV  28.2842 0.2745 1.0000
  seashell          = HSV  24.7376 0.0667 1.0000
  snow              = HSV   0.0000 0.0196 1.0000
  thistle           = HSV 300.0000 0.1158 0.8471
  titaniumWhite     = HSV  70.0000 0.0600 1.0000
  wheat             = HSV  39.0881 0.2694 0.9608
  white             = HSV   0.0000 0.0000 1.0000
  whiteSmoke        = HSV   0.0000 0.0000 0.9608
  zincWhite         = HSV 280.0000 0.0300 1.0000

  -- Greys
  coldGrey          = HSV 164.9999 0.0741 0.5400
  dimGrey           = HSV   0.0000 0.0000 0.4118
  grey              = HSV   0.0000 0.0000 0.7529
  lightGrey         = HSV   0.0000 0.0000 0.8275
  slateGrey         = HSV 209.9761 0.2222 0.5647
  slateGreyDark     = HSV 180.0000 0.4051 0.3098
  slateGreyLight    = HSV 210.0225 0.2222 0.6000
  warmGrey          = HSV  60.0000 0.1800 0.5000

  -- Blacks
  black             = HSV   0.0000 0.0000 0.0000
  ivoryBlack        = HSV  20.0000 0.1875 0.1600
  lampBlack         = HSV 150.0000 0.3571 0.2800

  -- Reds
  alizarinCrimson   = HSV 355.1351 0.8315 0.8900
  brick             = HSV  34.2857 0.8033 0.6100
  cadmiumRedDeep    = HSV   2.8571 0.9438 0.8900
  coral             = HSV  16.1125 0.6863 1.0000
  coralLight        = HSV   0.0000 0.4666 0.9412
  deepPink          = HSV 327.5716 0.9216 1.0000
  englishRed        = HSV  11.5068 0.8795 0.8300
  firebrick         = HSV   0.0000 0.8090 0.6980
  geraniumLake      = HSV 351.2195 0.9213 0.8900
  hotPink           = HSV 330.0000 0.5882 1.0000
  indianRed         = HSV 357.0000 0.8696 0.6900
  lightSalmon       = HSV  17.1511 0.5216 1.0000
  madderLakeDeep    = HSV 359.1549 0.7978 0.8900
  maroon            = HSV 337.4940 0.7273 0.6902
  pink              = HSV 349.5103 0.2471 1.0000
  pinkLight         = HSV 350.9466 0.2863 1.0000
  raspberry         = HSV 330.0000 0.7170 0.5300
  red               = HSV   0.0000 1.0000 1.0000
  roseMadder        = HSV 359.1177 0.7640 0.8900
  salmon            = HSV   6.1766 0.5440 0.9804
  tomato            = HSV   9.1297 0.7216 1.0000
  venetianRed       = HSV 358.3562 0.8795 0.8300

  -- Browns
  beige             = HSV  34.2857 0.2187 0.6400
  brown             = HSV   0.0000 0.6706 0.5000
  brownMadder       = HSV   0.0000 0.8140 0.8600
  brownOchre        = HSV  20.4878 0.7736 0.5300
  burlywood         = HSV  33.7984 0.3919 0.8706
  burntSienna       = HSV  18.7500 0.8889 0.5400
  burntUmber        = HSV   9.0000 0.7407 0.5400
  chocolate         = HSV  25.0064 0.8572 0.8235
  deepOchre         = HSV  24.0000 0.7778 0.4500
  flesh             = HSV  19.2000 0.7500 1.0000
  fleshOchre        = HSV  14.4828 0.8700 1.0000
  goldOchre         = HSV  30.4762 0.8077 0.7800
  greenishUmber     = HSV  12.0000 0.9500 1.0000
  khaki             = HSV  54.0031 0.4167 0.9412
  khakiDark         = HSV  55.5970 0.4339 0.7412
  lightBeige        = HSV  60.0000 0.1021 0.9608
  peru              = HSV  29.5797 0.6926 0.8039
  rosyBrown         = HSV   0.0000 0.2394 0.7373
  rawSienna         = HSV  25.7143 0.8974 0.7800
  rawUmber          = HSV  34.7368 0.8444 0.4500
  sepia             = HSV  16.0000 0.8108 0.3700
  sienna            = HSV  19.3038 0.7187 0.6275
  saddleBrown       = HSV  25.0021 0.8633 0.5451
  sandyBrown        = HSV  27.5603 0.6065 0.9569
  tan               = HSV  34.2951 0.3333 0.8235
  vanDykeBrown      = HSV  22.2857 0.9459 0.3700

  -- Oranges
  cadmiumOrange     = HSV  22.4242 0.9900 1.0000
  cadmiumRedLight   = HSV 357.5758 0.9900 1.0000
  carrot            = HSV  33.0000 0.8602 0.9300
  darkOrange        = HSV  32.9400 1.0000 1.0000
  marsOrange        = HSV  22.3529 0.8644 0.5900
  marsYellow        = HSV  25.8228 0.8876 0.8900
  orange            = HSV  30.0000 1.0000 1.0000
  orangeRed         = HSV  16.2360 1.0000 1.0000
  yellowOchre       = HSV  31.5000 0.8989 0.8900

  -- Yellows
  aureolineYellow   = HSV  36.2791 0.8600 1.0000
  banana            = HSV  51.2727 0.6180 0.8900
  cadmiumLemon      = HSV  53.3333 0.9900 1.0000
  cadmiumYellow     = HSV  34.1936 0.9300 1.0000
  gold              = HSV  50.5860 1.0000 1.0000
  goldenrod         = HSV  42.9065 0.8532 0.8549
  goldenrodDark     = HSV  42.6588 0.9403 0.7216
  goldenrodLight    = HSV  60.0000 0.1600 0.9804
  goldenrodPale     = HSV  54.7112 0.2857 0.9333
  lightGoldenrod    = HSV  50.5643 0.4538 0.9333
  melon             = HSV  31.2500 0.5393 0.8900
  naplesYellowDeep  = HSV  38.0645 0.9300 1.0000
  yellow            = HSV  60.0000 1.0000 1.0000
  yellowLight       = HSV  60.0000 0.1216 1.0000

  -- Greens
  chartreuse        = HSV  90.1200 1.0000 1.0000
  chromeoxideGreen  = HSV  74.2857 0.8400 0.5000
  cinnabarGreen     = HSV  95.5556 0.7714 0.7000
  cobaltGreen       = HSV 121.8182 0.5789 0.5700
  emeraldGreen      = HSV 145.8228 1.0000 0.7900
  forestGreen       = HSV 120.0000 0.7555 0.5451
  green             = HSV 120.0000 1.0000 1.0000
  greenDark         = HSV 120.0000 1.0000 0.3922
  greenPale         = HSV 120.0000 0.3944 0.9843
  greenYellow       = HSV  83.6558 0.8157 1.0000
  lawnGreen         = HSV  90.4736 1.0000 0.9882
  limeGreen         = HSV 120.0000 0.7561 0.8039
  mint              = HSV 132.0000 0.2525 0.9900
  olive             = HSV 102.0000 0.5405 0.3700
  oliveDrab         = HSV  79.6330 0.7535 0.5569
  oliveGreenDark    = HSV  82.0060 0.5608 0.4196
  permanentGreen    = HSV 130.4000 0.9494 0.7900
  sapGreen          = HSV 104.2857 0.8400 0.5000
  seaGreen          = HSV 146.4546 0.6691 0.5451
  seaGreenDark      = HSV 120.0000 0.2394 0.7373
  seaGreenMedium    = HSV 146.7152 0.6648 0.7020
  seaGreenLight     = HSV 176.7196 0.8202 0.6980
  springGreen       = HSV 149.8800 1.0000 1.0000
  springGreenMedium = HSV 156.9584 1.0000 0.9804
  terreVerte        = HSV  89.0323 0.8378 0.3700
  viridianLight     = HSV 121.0526 0.5700 1.0000
  yellowGreen       = HSV  79.7433 0.7561 0.8039

  -- Cyans
  aquamarine        = HSV 159.8486 0.5020 1.0000
  aquamarineMedium  = HSV 159.6187 0.5024 0.8039
  cyan              = HSV 180.0000 1.0000 1.0000
  cyanWhite         = HSV 180.0000 0.1216 1.0000
  turquoise         = HSV 174.0038 0.7143 0.8784
  turquoiseDark     = HSV 180.8638 1.0000 0.8196
  turquoiseMedium   = HSV 177.8109 0.6554 0.8196
  turquoisePale     = HSV 180.0000 0.2647 0.9333

  -- Blues
  aliceBlue         = HSV 208.0612 0.0588 1.0000
  blue              = HSV 240.0000 1.0000 1.0000
  blueLight         = HSV 194.7317 0.2479 0.9020
  blueMedium        = HSV 240.0000 1.0000 0.8039
  cadet             = HSV 181.8588 0.4064 0.6275
  cobalt            = HSV 224.6512 0.6418 0.6700
  cornflower        = HSV 218.5443 0.5780 0.9294
  cerulean          = HSV 186.1538 0.9750 0.8000
  dodgerBlue        = HSV 209.5988 0.8824 1.0000
  indigo            = HSV 210.0000 0.9091 0.3300
  manganeseBlue     = HSV 176.3077 0.9848 0.6600
  midnightBlue      = HSV 240.0000 0.7769 0.4392
  navy              = HSV 240.0000 1.0000 0.5020
  peacock           = HSV 196.2712 0.7468 0.7900
  powderBlue        = HSV 186.6856 0.2348 0.9020
  royalBlue         = HSV 224.9976 0.7111 0.8824
  slateBlue         = HSV 248.3548 0.5610 0.8039
  slateBlueDark     = HSV 248.4733 0.5612 0.5451
  slateBlueLight    = HSV 248.3880 0.5608 1.0000
  slateBlueMedium   = HSV 248.5176 0.5631 0.9333
  skyBlue           = HSV 197.4095 0.4256 0.9216
  skyBlueDeep       = HSV 195.0600 1.0000 1.0000
  skyBlueLight      = HSV 202.9623 0.4600 0.9804
  steelBlue         = HSV 207.2740 0.6111 0.7059
  steelBlueLight    = HSV 213.9246 0.2072 0.8706
  turquoiseBlue     = HSV 162.3077 1.0000 0.7800
  ultramarine       = HSV 243.4616 0.9286 0.5600

  -- Magentas
  blueViolet        = HSV 271.1495 0.8098 0.8863
  cobaltVioletDeep  = HSV 293.8776 0.7903 0.6200
  magenta           = HSV 300.0000 1.0000 1.0000
  orchid            = HSV 302.2660 0.4863 0.8549
  orchidDark        = HSV 280.1292 0.7549 0.8000
  orchidMedium      = HSV 288.0898 0.5972 0.8275
  permanentViolet   = HSV 349.8592 0.8256 0.8600
  plum              = HSV 300.0000 0.2760 0.8667
  purple            = HSV 276.9253 0.8667 0.9412
  purpleMedium      = HSV 259.6330 0.4886 0.8588
  ultramarineViolet = HSV 285.5172 0.6744 0.4300
  violet            = HSV 289.5652 0.3833 0.6000
  violetDark        = HSV 282.0834 1.0000 0.8275
  violetRed         = HSV 321.8198 0.8461 0.8157
  violetRedMedium   = HSV 322.2464 0.8944 0.7804
  violetRedPale     = HSV 340.3670 0.4886 0.8588

