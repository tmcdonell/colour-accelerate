{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.HSL
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Colours in the HSL (hue-saturation-lightness) cylindrical-coordinate
-- representation of points in the RGB colour space.
--

module Data.Array.Accelerate.Data.Colour.HSL (

  Colour,
  HSL(..),

  hsl,
  toRGB, fromRGB,
  hue,
  saturation,
  lightness,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.RGB    ( RGB(..) )
import Data.Array.Accelerate.Data.Colour.Names  as C

import Data.Typeable
import Prelude                                  as P


-- | A HSL colour value
--
type Colour = HSL Float

-- | Construct a HSL colour value from the individual channel components. The
-- hue component is measured in degrees and wrapped to the range [0..360), while
-- the saturation and value are clamped to the range [0..1].
--
hsl :: Exp Float        -- ^ hue component
    -> Exp Float        -- ^ saturation component
    -> Exp Float        -- ^ lightness component
    -> Exp Colour
hsl h s l
  = clamp
  $ lift (HSL h s l)


clamp :: Exp Colour -> Exp Colour
clamp (unlift -> HSL h s l)
  = lift
  $ HSL (fmod h 360) (c s) (c l)
  where
    c x = 0 `A.max` x `A.min` 1

fmod :: Exp Float -> Exp Float -> Exp Float
fmod n d = n - f * d
  where
    f = A.fromIntegral (A.floor (n / d) :: Exp Int)

-- | Convert a HSL colour to an RGB colour-space value
--
toRGB :: Exp (HSL Float) -> Exp (RGB Float)
toRGB (unlift -> HSL h s l) = rgb
  where
    c   = (1 - abs (2*l-1)) * s
    h'  = h / 60
    x   = c * (1 - abs ((h' `fmod` 2) - 1))
    -- --
    m   = l - 0.5*c
    c'  = c + m
    x'  = x + m
    --
    rgb = h' A.<* 1 ? ( lift (RGB c' x' m)
        , h' A.<* 2 ? ( lift (RGB x' c' m)
        , h' A.<* 3 ? ( lift (RGB m  c' x')
        , h' A.<* 4 ? ( lift (RGB m  x' c')
        , h' A.<* 5 ? ( lift (RGB x' m  c')
        ,             ( lift (RGB c' m  x') ))))))


-- | Convert a point in the RGB colour-space to a point in the HSL colour-space.
--
fromRGB :: Exp (RGB Float) -> Exp (HSL Float)
fromRGB (unlift -> RGB r g b) = lift (HSL h s l)
  where
    mx = P.maximum [r,g,b]
    mn = P.minimum [r,g,b]
    c  = mx - mn
    --
    l  = 0.5 * (mx + mn)
    s  = c A.==* 0 ? ( 0, c / (1 - abs (2*l-1)) )
    h  = c A.==* 0 ? ( 0, h0 * 60 )
    --
    h0 = mx ==* r ? ( ((g-b)/c) `fmod` 6
       , mx ==* g ? ( ((b-r)/c) + 2
       , mx ==* b ? ( ((r-g)/c) + 4
       , {- otherwise -} 0 )))


-- | Return the HSL-hue of an RGB colour
--
hue :: Exp (RGB Float) -> Exp Float
hue (unlift . fromRGB -> HSL h _ _) = h

-- | Return the HSL-saturation of an RGB colour
--
saturation :: Exp (RGB Float) -> Exp Float
saturation (unlift . fromRGB -> HSL _ s _) = s

-- | Return the HSL-lightness of an RGB colour
--
lightness :: Exp (RGB Float) -> Exp Float
lightness (unlift . fromRGB -> HSL _ _ l) = l


-- Accelerate bits
-- ---------------

-- HSL colour space
--
data HSL a = HSL a a a
  deriving (Show, P.Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 3-tuple
--
type instance EltRepr (HSL a) = EltRepr (a, a, a)

instance Elt a => Elt (HSL a) where
  eltType (_ :: HSL a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (h,s,l) = toElt c in HSL h s l
  fromElt (HSL h s l)           = fromElt (h,s,l)

instance Elt a => IsProduct Elt (HSL a) where
  type ProdRepr (HSL a)          = ((((),a), a), a)
  fromProd _ (HSL h s l)         = ((((), h), s), l)
  toProd _ ((((),h),s),l)        = HSL h s l
  prod cst _                     = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (HSL a) where
  type Plain (HSL a)    = HSL (Plain a)
  lift (HSL h s l)      = Exp . Tuple $ NilTup `SnocTup` lift h `SnocTup` lift s `SnocTup` lift l

instance Elt a => Unlift Exp (HSL (Exp a)) where
  unlift c      = let h = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      s = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      l = Exp $ ZeroTupIdx `Prj` c
                  in HSL h s l

instance P.Num a => P.Num (HSL a) where
  (+) (HSL h1 s1 v1 ) (HSL h2 s2 v2)
        = HSL (h1 + h2) (s1 + s2) (v1 + v2)

  (-) (HSL h1 s1 v1) (HSL h2 s2 v2)
        = HSL (h1 - h2) (s1 - s2) (v1 - v2)

  (*) (HSL h1 s1 v1) (HSL h2 s2 v2)
        = HSL (h1 * h2) (s1 * s2) (v1 * v2)

  abs (HSL h1 s1 v1)
        = HSL (abs h1) (abs s1) (abs v1)

  signum (HSL h1 s1 v1)
        = HSL (signum h1) (signum s1) (signum v1)

  fromInteger i
        = let f = fromInteger i
          in  HSL f f f

instance (P.Num a, P.Fractional a) => P.Fractional (HSL a) where
  (/) (HSL h1 s1 l1) (HSL h2 s2 l2)
        = HSL (h1/h2) (s1/s2) (l1/l2)

  recip (HSL h1 s1 l1)
        = HSL (recip h1) (recip s1) (recip l1)

  fromRational r
        = let f = fromRational r
          in  HSL f f f


instance {-# OVERLAPS #-} A.Num a => P.Num (Exp (HSL a)) where
  (+)           = lift2 ((+) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  (-)           = lift2 ((-) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  (*)           = lift2 ((*) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  abs           = lift1 (abs :: HSL (Exp a) -> HSL (Exp a))
  signum        = lift1 (signum :: HSL (Exp a) -> HSL (Exp a))
  fromInteger i = let f = fromInteger i :: Exp a
                  in lift $ HSL f f f

instance {-# OVERLAPS #-} A.Fractional a => P.Fractional (Exp (HSL a)) where
  (/)            = lift2 ((/) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  recip          = lift1 (recip :: HSL (Exp a) -> HSL (Exp a))
  fromRational r = let f = fromRational r :: Exp a
                   in lift $ HSL f f f


-- Named colours
-- -------------

instance NamedColour (HSL Float) where
  -- Whites
  antiqueWhite      = HSL  34.3044 0.7779 0.9118
  azure             = HSL 180.0000 1.0000 0.9706
  bisque            = HSL  32.5411 1.0000 0.8843
  blanchedAlmond    = HSL  36.0122 1.0000 0.9020
  cornsilk          = HSL  47.9825 1.0000 0.9314
  eggshell          = HSL  33.0000 0.9091 0.8900
  floralWhite       = HSL  40.0000 1.0000 0.9706
  gainsboro         = HSL   0.0000 0.0000 0.8627
  ghostWhite        = HSL 240.0000 1.0000 0.9863
  honeydew          = HSL 120.0000 1.0000 0.9706
  ivory             = HSL  60.0000 1.0000 0.9706
  lavender          = HSL 240.0000 0.6667 0.9412
  lavenderBlush     = HSL 340.0000 1.0000 0.9706
  lemonChiffon      = HSL  54.0031 1.0000 0.9020
  linen             = HSL  30.0000 0.6667 0.9412
  mintCream         = HSL 150.0000 1.0000 0.9804
  mistyRose         = HSL   5.9694 1.0000 0.9412
  moccasin          = HSL  38.1048 1.0000 0.8549
  navajoWhite       = HSL  35.8582 1.0000 0.8392
  oldLace           = HSL  39.1131 0.8526 0.9471
  papayaWhip        = HSL  37.1585 1.0000 0.9176
  peachPuff         = HSL  28.2842 1.0000 0.8628
  seashell          = HSL  24.7376 1.0000 0.9666
  snow              = HSL   0.0000 1.0000 0.9902
  thistle           = HSL 300.0000 0.2429 0.7981
  titaniumWhite     = HSL  70.0000 1.0000 0.9700
  wheat             = HSL  39.0881 0.7675 0.8314
  white             = HSL   0.0000 0.0000 1.0000
  whiteSmoke        = HSL   0.0000 0.0000 0.9608
  zincWhite         = HSL 280.0000 1.0000 0.9850

  -- Greys
  coldGrey          = HSL 164.9999 0.0417 0.5200
  dimGrey           = HSL   0.0000 0.0000 0.4118
  grey              = HSL   0.0000 0.0000 0.7529
  lightGrey         = HSL   0.0000 0.0000 0.8275
  slateGrey         = HSL 209.9761 0.1260 0.5020
  slateGreyDark     = HSL 180.0000 0.2540 0.2470
  slateGreyLight    = HSL 210.0225 0.1428 0.5334
  warmGrey          = HSL  60.0000 0.0989 0.4550

  -- Blacks
  black             = HSL   0.0000 0.0000 0.0000
  ivoryBlack        = HSL  20.0000 0.1034 0.1450
  lampBlack         = HSL 150.0000 0.2174 0.2300

  -- Reds
  alizarinCrimson   = HSL 355.1351 0.7708 0.5200
  brick             = HSL  34.2857 0.6712 0.3650
  cadmiumRedDeep    = HSL   2.8571 0.8936 0.4700
  coral             = HSL  16.1125 1.0000 0.6568
  coralLight        = HSL   0.0000 0.7888 0.7216
  deepPink          = HSL 327.5716 1.0000 0.5392
  englishRed        = HSL  11.5068 0.7849 0.4650
  firebrick         = HSL   0.0000 0.6793 0.4156
  geraniumLake      = HSL 351.2195 0.8542 0.4800
  hotPink           = HSL 330.0000 1.0000 0.7059
  indianRed         = HSL 357.0000 0.7692 0.3900
  lightSalmon       = HSL  17.1511 1.0000 0.7392
  madderLakeDeep    = HSL 359.1549 0.7634 0.5350
  maroon            = HSL 337.4940 0.5715 0.4392
  pink              = HSL 349.5103 1.0000 0.8764
  pinkLight         = HSL 350.9466 1.0000 0.8568
  raspberry         = HSL 330.0000 0.5588 0.3400
  red               = HSL   0.0000 1.0000 0.5000
  roseMadder        = HSL 359.1177 0.7556 0.5500
  salmon            = HSL   6.1766 0.9315 0.7138
  tomato            = HSL   9.1297 1.0000 0.6392
  venetianRed       = HSL 358.3562 0.7849 0.4650

  -- Browns
  beige             = HSL  34.2857 0.1628 0.5700
  brown             = HSL   0.0000 0.5044 0.3324
  brownMadder       = HSL   0.0000 0.7143 0.5100
  brownOchre        = HSL  20.4878 0.6308 0.3250
  burlywood         = HSL  33.7984 0.5687 0.7000
  burntSienna       = HSL  18.7500 0.8000 0.3000
  burntUmber        = HSL   9.0000 0.5882 0.3400
  chocolate         = HSL  25.0064 0.7501 0.4706
  deepOchre         = HSL  24.0000 0.6364 0.2750
  flesh             = HSL  19.2000 1.0000 0.6250
  fleshOchre        = HSL  14.4828 1.0000 0.5650
  goldOchre         = HSL  30.4762 0.6774 0.4650
  greenishUmber     = HSL  12.0000 1.0000 0.5250
  khaki             = HSL  54.0031 0.7693 0.7451
  khakiDark         = HSL  55.5970 0.3832 0.5804
  lightBeige        = HSL  60.0000 0.5558 0.9117
  peru              = HSL  29.5797 0.5867 0.5255
  rosyBrown         = HSL   0.0000 0.2515 0.6490
  rawSienna         = HSL  25.7143 0.8140 0.4300
  rawUmber          = HSL  34.7368 0.7308 0.2600
  sepia             = HSL  16.0000 0.6818 0.2200
  sienna            = HSL  19.3038 0.5609 0.4020
  saddleBrown       = HSL  25.0021 0.7595 0.3098
  sandyBrown        = HSL  27.5603 0.8707 0.6667
  tan               = HSL  34.2951 0.4375 0.6862
  vanDykeBrown      = HSL  22.2857 0.8974 0.1950

  -- Oranges
  cadmiumOrange     = HSL  22.4242 1.0000 0.5050
  cadmiumRedLight   = HSL 357.5758 1.0000 0.5050
  carrot            = HSL  33.0000 0.8511 0.5300
  darkOrange        = HSL  32.9400 1.0000 0.5000
  marsOrange        = HSL  22.3529 0.7612 0.3350
  marsYellow        = HSL  25.8228 0.7980 0.4950
  orange            = HSL  30.0000 1.0000 0.5000
  orangeRed         = HSL  16.2360 1.0000 0.5000
  yellowOchre       = HSL  31.5000 0.8163 0.4900

  -- Yellows
  aureolineYellow   = HSL  36.2791 1.0000 0.5700
  banana            = HSL  51.2727 0.7143 0.6150
  cadmiumLemon      = HSL  53.3333 1.0000 0.5050
  cadmiumYellow     = HSL  34.1936 1.0000 0.5350
  gold              = HSL  50.5860 1.0000 0.5000
  goldenrod         = HSL  42.9065 0.7440 0.4902
  goldenrodDark     = HSL  42.6588 0.8873 0.3824
  goldenrodLight    = HSL  60.0000 0.8001 0.9020
  goldenrodPale     = HSL  54.7112 0.6665 0.8000
  lightGoldenrod    = HSL  50.5643 0.7605 0.7216
  melon             = HSL  31.2500 0.6857 0.6500
  naplesyellowdeep  = HSL  38.0645 1.0000 0.5350
  yellow            = HSL  60.0000 1.0000 0.5000
  yellowLight       = HSL  60.0000 1.0000 0.9392

  -- Greens
  chartreuse        = HSL  90.1200 1.0000 0.5000
  chromeoxidegreen  = HSL  74.2857 0.7241 0.2900
  cinnabarGreen     = HSL  95.5556 0.6279 0.4300
  cobaltGreen       = HSL 121.8182 0.4074 0.4050
  emeraldGreen      = HSL 145.8228 1.0000 0.3950
  forestGreen       = HSL 120.0000 0.6070 0.3392
  green             = HSL 120.0000 1.0000 0.5000
  greenDark         = HSL 120.0000 1.0000 0.1961
  greenPale         = HSL 120.0000 0.9252 0.7902
  greenYellow       = HSL  83.6558 1.0000 0.5922
  lawnGreen         = HSL  90.4736 1.0000 0.4941
  limeGreen         = HSL 120.0000 0.6078 0.5000
  mint              = HSL 132.0000 0.9259 0.8650
  olive             = HSL 102.0000 0.3704 0.2700
  oliveDrab         = HSL  79.6330 0.6044 0.3471
  oliveGreenDark    = HSL  82.0060 0.3896 0.3020
  permanentGreen    = HSL 130.4000 0.9036 0.4150
  sapGreen          = HSL 104.2857 0.7241 0.2900
  seaGreen          = HSL 146.4546 0.5027 0.3628
  seaGreenDark      = HSL 120.0000 0.2515 0.6490
  seaGreenMedium    = HSL 146.7152 0.4979 0.4686
  seaGreenLight     = HSL 176.7196 0.6952 0.4118
  springGreen       = HSL 149.8800 1.0000 0.5000
  springGreenmedium = HSL 156.9584 1.0000 0.4902
  terreVerte        = HSL  89.0323 0.7209 0.2150
  viridianLight     = HSL 121.0526 1.0000 0.7150
  yellowGreen       = HSL  79.7433 0.6078 0.5000

  -- Cyans
  aquamarine        = HSL 159.8486 1.0000 0.7490
  aquamarinemedium  = HSL 159.6187 0.5073 0.6020
  cyan              = HSL 180.0000 1.0000 0.5000
  cyanWhite         = HSL 180.0000 1.0000 0.9392
  turquoise         = HSL 174.0038 0.7207 0.5647
  turquoiseDark     = HSL 180.8638 1.0000 0.4098
  turquoiseMedium   = HSL 177.8109 0.5982 0.5510
  turquoisePale     = HSL 180.0000 0.6493 0.8098

  -- Blues
  aliceBlue         = HSL 208.0612 1.0000 0.9706
  blue              = HSL 240.0000 1.0000 0.5000
  blueLight         = HSL 194.7317 0.5329 0.7902
  blueMedium        = HSL 240.0000 1.0000 0.4020
  cadet             = HSL 181.8588 0.2550 0.5000
  cobalt            = HSL 224.6512 0.4725 0.4550
  cornflower        = HSL 218.5443 0.7919 0.6608
  cerulean          = HSL 186.1538 0.9512 0.4100
  dodgerBlue        = HSL 209.5988 1.0000 0.5588
  indigo            = HSL 210.0000 0.8333 0.1800
  manganeseBlue     = HSL 176.3077 0.9701 0.3350
  midnightBlue      = HSL 240.0000 0.6351 0.2686
  navy              = HSL 240.0000 1.0000 0.2510
  peacock           = HSL 196.2712 0.5960 0.4950
  powderBlue        = HSL 186.6856 0.5194 0.7961
  royalBlue         = HSL 224.9976 0.7274 0.5686
  slateBlue         = HSL 248.3548 0.5349 0.5784
  slateBlueDark     = HSL 248.4733 0.3900 0.3921
  slateBlueLight    = HSL 248.3880 1.0000 0.7196
  slateBlueMedium   = HSL 248.5176 0.7975 0.6706
  skyBlue           = HSL 197.4095 0.7144 0.7255
  skyBlueDeep       = HSL 195.0600 1.0000 0.5000
  skyBlueLight      = HSL 202.9623 0.9200 0.7549
  steelBlue         = HSL 207.2740 0.4400 0.4902
  steelBlueLight    = HSL 213.9246 0.4107 0.7804
  turquoiseBlue     = HSL 162.3077 1.0000 0.3900
  ultramarine       = HSL 243.4616 0.8667 0.3000

  -- Magentas
  blueViolet        = HSL 271.1495 0.7594 0.5274
  cobaltVioletdeep  = HSL 293.8776 0.6533 0.3750
  magenta           = HSL 300.0000 1.0000 0.5000
  orchid            = HSL 302.2660 0.5889 0.6470
  orchidDark        = HSL 280.1292 0.6063 0.4980
  orchidMedium      = HSL 288.0898 0.5889 0.5804
  permanentViolet   = HSL 349.8592 0.7172 0.5050
  plum              = HSL 300.0000 0.4729 0.7471
  purple            = HSL 276.9253 0.8740 0.5334
  purpleMedium      = HSL 259.6330 0.5977 0.6490
  ultramarineViolet = HSL 285.5172 0.5088 0.2850
  violet            = HSL 289.5652 0.2371 0.4850
  violetDark        = HSL 282.0834 1.0000 0.4138
  violetRed         = HSL 321.8198 0.7333 0.4706
  violetRedmedium   = HSL 322.2464 0.8090 0.4314
  violetRedPale     = HSL 340.3670 0.5977 0.6490

