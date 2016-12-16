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
-- Module      : Data.Array.Accelerate.Data.Colour.RGB
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- RGB triples for an unspecified colour space
--

module Data.Array.Accelerate.Data.Colour.RGB (

  Colour,
  RGB(..),

  rgb, rgb8,
  clamp,
  blend,
  luminance,

  packRGB,  packBGR,  unpackRGB,  unpackBGR,
  packRGB8, packBGR8, unpackRGB8, unpackBGR8,

) where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product                      ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar                  ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.Names
import Data.Array.Accelerate.Data.Colour.Internal.Pack

import Data.Typeable
import Prelude                                            as P


-- | An RGB colour value
--
type Colour = RGB Float

-- | Construct an RGB colour from individual channel components. The components
-- will be clamped to the range [0..1].
--
rgb :: Exp Float        -- ^ red component
    -> Exp Float        -- ^ green component
    -> Exp Float        -- ^ blue component
    -> Exp Colour
rgb r g b
  = clamp
  $ lift (RGB r g b)


-- | Construct a colour from 8-bit-per-channel colour components.
--
rgb8 :: Exp Word8       -- ^ red component
     -> Exp Word8       -- ^ green component
     -> Exp Word8       -- ^ blue component
     -> Exp Colour
rgb8 r g b
  = lift
  $ RGB (A.fromIntegral r / 255)
        (A.fromIntegral g / 255)
        (A.fromIntegral b / 255)


-- | Clamp each component of a colour to the range [0..1].
--
clamp :: Exp Colour -> Exp Colour
clamp = lift1 (fmap c :: RGB (Exp Float) -> RGB (Exp Float))
  where
    c x = 0 `A.max` x `A.min` 1


-- | Blend two colours in the given proportions.
--
-- Note that this uses an approximation of gamma=2 (i.e. sum-of-squares method).
-- It is recommended to instead convert to the sRGB colour space if you want
-- more accurate colour blending, or if you intend to use the gamma-corrected
-- values more than once (e.g. in a stencil).
--
-- > blend c1 c2 ~= SRGB.toRGB ( (SRGB.fromRGB c1 + SRGB.fromRGB c2) / 2 )
--
-- See the Blur program in the examples for a comparison of mixing colours in
-- the RGB and sRGB colour spaces.
--
blend :: Exp Float      -- ^ Proportion of first colour
      -> Exp Float      -- ^ Proportion of second colour
      -> Exp Colour     -- ^ First colour
      -> Exp Colour     -- ^ Second colour
      -> Exp Colour     -- ^ Resulting colour
blend m1 m2 c1 c2 =
  let
      RGB r1 g1 b1    = unlift c1
      RGB r2 g2 b2    = unlift c2

      -- Normalise mixing proportions to ratios.
      m12 = m1 + m2
      m1' = m1 / m12
      m2' = m2 / m12

      -- Colours components should be added via sum of squares, otherwise the
      -- result will be too dark.
      r1s = r1 * r1;    r2s = r2 * r2
      g1s = g1 * g1;    g2s = g2 * g2
      b1s = b1 * b1;    b2s = b2 * b2
  in
  rgb (sqrt (m1' * r1s + m2' * r2s))
      (sqrt (m1' * g1s + m2' * g2s))
      (sqrt (m1' * b1s + m2' * b2s))


-- | Luminance of an RGB colour (Y component of a YUV colour).
--
luminance :: Exp Colour -> Exp Float
luminance (unlift -> RGB r g b) = 0.299*r + 0.587*g + 0.114*b


-- Packed representation
-- ---------------------

-- | Convert a Colour into a packed-word RGBA representation
--
packRGB :: Exp Colour -> Exp Word32
packRGB (unlift -> RGB r g b) = pack8 (word8OfFloat r) (word8OfFloat g) (word8OfFloat b) 0xFF

-- | Convert a colour into a packed-word ABGR representation
--
packBGR :: Exp Colour -> Exp Word32
packBGR (unlift -> RGB r g b) = pack8 0xFF (word8OfFloat b) (word8OfFloat g) (word8OfFloat r)

packRGB8 :: Exp (RGB Word8) -> Exp Word32
packRGB8 (unlift -> RGB r g b) = pack8 r g b 0xFF

packBGR8 :: Exp (RGB Word8) -> Exp Word32
packBGR8 (unlift -> RGB r g b) = pack8 0xff b g r


-- | Convert a colour from a packed-word RGBA representation
--
unpackRGB :: Exp Word32 -> Exp Colour
unpackRGB w =
  let (r,g,b::Exp Word8,_::Exp Word8) = unlift (unpack8 w)
  in  rgb8 r g b

-- | Convert a colour from a packed-word ABGR representation
--
unpackBGR :: Exp Word32 -> Exp Colour
unpackBGR w =
  let (_::Exp Word8,b::Exp Word8,g,r) = unlift (unpack8 w)
  in  rgb8 r g b

unpackRGB8 :: Exp Word32 -> Exp (RGB Word8)
unpackRGB8 w =
  let (r,g,b::Exp Word8,_::Exp Word8) = unlift (unpack8 w)
  in  lift $ RGB r g b

unpackBGR8 :: Exp Word32 -> Exp (RGB Word8)
unpackBGR8 w =
  let (_::Exp Word8,b::Exp Word8,g,r) = unlift (unpack8 w)
  in  lift $ RGB r g b


-- Accelerate bits
-- ---------------

-- RGB colour space
--
data RGB a = RGB a a a
  deriving (Show, P.Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 3-tuple
--
type instance EltRepr (RGB a) = EltRepr (a, a, a)

instance Elt a => Elt (RGB a) where
  eltType (_ :: RGB a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (r,g,b) = toElt c in RGB r g b
  fromElt (RGB r g b)           = fromElt (r,g,b)

instance Elt a => IsProduct Elt (RGB a) where
  type ProdRepr (RGB a)          = ((((),a), a), a)
  fromProd _ (RGB r g b)         = ((((), r), g), b)
  toProd _ ((((),r),g),b)        = RGB r g b
  prod cst _                     = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGB a) where
  type Plain (RGB a)    = RGB (Plain a)
  lift (RGB r g b)      = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g `SnocTup` lift b

instance Elt a => Unlift Exp (RGB (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      g = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      b = Exp $ ZeroTupIdx `Prj` c
                  in RGB r g b

instance P.Num a => P.Num (RGB a) where
  (+) (RGB r1 g1 b1 ) (RGB r2 g2 b2)
        = RGB (r1 + r2) (g1 + g2) (b1 + b2)

  (-) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 - r2) (g1 - g2) (b1 - b2)

  (*) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 * r2) (g1 * g2) (b1 * b2)

  abs (RGB r1 g1 b1)
        = RGB (abs r1) (abs g1) (abs b1)

  signum (RGB r1 g1 b1)
        = RGB (signum r1) (signum g1) (signum b1)

  fromInteger i
        = let f = fromInteger i
          in  RGB f f f

instance (P.Num a, P.Fractional a) => P.Fractional (RGB a) where
  (/) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1/r2) (g1/g2) (b1/b2)

  recip (RGB r1 g1 b1)
        = RGB (recip r1) (recip g1) (recip b1)

  fromRational r
        = let f = fromRational r
          in  RGB f f f


instance {-# OVERLAPS #-} A.Num a => P.Num (Exp (RGB a)) where
  (+)           = lift2 ((+) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (-)           = lift2 ((-) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (*)           = lift2 ((*) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  abs           = lift1 (abs :: RGB (Exp a) -> RGB (Exp a))
  signum        = lift1 (signum :: RGB (Exp a) -> RGB (Exp a))
  fromInteger i = let f = fromInteger i :: Exp a
                  in  lift $ RGB f f f

instance {-# OVERLAPS #-} A.Fractional a => P.Fractional (Exp (RGB a)) where
  (/)            = lift2 ((/) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  recip          = lift1 (recip :: RGB (Exp a) -> RGB (Exp a))
  fromRational r = let f = fromRational r :: Exp a
                   in lift $ RGB f f f


-- Named colours
-- -------------

instance NamedColour (RGB Word8) where
  -- Whites
  antiqueWhite      = RGB 250 235 215
  azure             = RGB 240 255 255
  bisque            = RGB 255 228 196
  blanchedAlmond    = RGB 255 235 205
  cornsilk          = RGB 255 248 220
  eggshell          = RGB 252 230 201
  floralWhite       = RGB 255 250 240
  gainsboro         = RGB 220 220 220
  ghostWhite        = RGB 248 248 255
  honeydew          = RGB 240 255 240
  ivory             = RGB 255 255 240
  lavender          = RGB 230 230 250
  lavenderBlush     = RGB 255 240 245
  lemonChiffon      = RGB 255 250 205
  linen             = RGB 250 240 230
  mintCream         = RGB 245 255 250
  mistyRose         = RGB 255 228 225
  moccasin          = RGB 255 228 181
  navajoWhite       = RGB 255 222 173
  oldLace           = RGB 253 245 230
  papayaWhip        = RGB 255 239 213
  peachPuff         = RGB 255 218 185
  seashell          = RGB 255 245 238
  snow              = RGB 255 250 250
  thistle           = RGB 216 191 216
  titaniumWhite     = RGB 252 255 240
  wheat             = RGB 245 222 179
  white             = RGB 255 255 255
  whiteSmoke        = RGB 245 245 245
  zincWhite         = RGB 253 248 255

  -- Greys
  coldGrey          = RGB 128 138 135
  dimGrey           = RGB 105 105 105
  grey              = RGB 192 192 192
  lightGrey         = RGB 211 211 211
  slateGrey         = RGB 112 128 144
  slateGreyDark     = RGB 47  79  79
  slateGreyLight    = RGB 119 136 153
  warmGrey          = RGB 128 128 105

  -- Blacks
  black             = RGB 0   0   0
  ivoryBlack        = RGB 41  36  33
  lampBlack         = RGB 46  71  59

  -- Reds
  alizarinCrimson   = RGB 227 38  54
  brick             = RGB 156 102 31
  cadmiumRedDeep    = RGB 227 23  13
  coral             = RGB 255 127 80
  coralLight        = RGB 240 128 128
  deepPink          = RGB 255 20  147
  englishRed        = RGB 212 61  26
  firebrick         = RGB 178 34  34
  geraniumLake      = RGB 227 18  48
  hotPink           = RGB 255 105 180
  indianRed         = RGB 176 23  31
  lightSalmon       = RGB 255 160 122
  madderLakeDeep    = RGB 227 46  48
  maroon            = RGB 176 48  96
  pink              = RGB 255 192 203
  pinkLight         = RGB 255 182 193
  raspberry         = RGB 135 38  87
  red               = RGB 255 0   0
  roseMadder        = RGB 227 54  56
  salmon            = RGB 250 128 114
  tomato            = RGB 255 99  71
  venetianRed       = RGB 212 26  31

  -- Browns
  beige             = RGB 163 148 128
  brown             = RGB 128 42  42
  brownMadder       = RGB 219 41  41
  brownOchre        = RGB 135 66  31
  burlywood         = RGB 222 184 135
  burntSienna       = RGB 138 54  15
  burntUmber        = RGB 138 51  36
  chocolate         = RGB 210 105 30
  deepOchre         = RGB 115 61  26
  flesh             = RGB 255 125 64
  fleshOchre        = RGB 255 87  33
  goldOchre         = RGB 199 120 38
  greenishUmber     = RGB 255 61  13
  khaki             = RGB 240 230 140
  khakiDark         = RGB 189 183 107
  lightBeige        = RGB 245 245 220
  peru              = RGB 205 133 63
  rosyBrown         = RGB 188 143 143
  rawSienna         = RGB 199 97  20
  rawUmber          = RGB 115 74  18
  sepia             = RGB 94  38  18
  sienna            = RGB 160 82  45
  saddleBrown       = RGB 139 69  19
  sandyBrown        = RGB 244 164 96
  tan               = RGB 210 180 140
  vanDykeBrown      = RGB 94  38  5

  -- Oranges
  cadmiumOrange     = RGB 255 97  3
  cadmiumRedLight   = RGB 255 3   13
  carrot            = RGB 237 145 33
  darkOrange        = RGB 255 140 0
  marsOrange        = RGB 150 69  20
  marsYellow        = RGB 227 112 26
  orange            = RGB 255 128 0
  orangeRed         = RGB 255 69  0
  yellowOchre       = RGB 227 130 23

  -- Yellows
  aureolineYellow   = RGB 255 168 36
  banana            = RGB 227 207 87
  cadmiumLemon      = RGB 255 227 3
  cadmiumYellow     = RGB 255 153 18
  gold              = RGB 255 215 0
  goldenrod         = RGB 218 165 32
  goldenrodDark     = RGB 184 134 11
  goldenrodLight    = RGB 250 250 210
  goldenrodPale     = RGB 238 232 170
  lightGoldenrod    = RGB 238 221 130
  melon             = RGB 227 168 105
  naplesYellowDeep  = RGB 255 168 18
  yellow            = RGB 255 255 0
  yellowLight       = RGB 255 255 224

  -- Greens
  chartreuse        = RGB 127 255 0
  chromeoxideGreen  = RGB 102 128 20
  cinnabarGreen     = RGB 97  179 41
  cobaltGreen       = RGB 61  145 64
  emeraldGreen      = RGB 0   201 87
  forestGreen       = RGB 34  139 34
  green             = RGB 0   255 0
  greenDark         = RGB 0   100 0
  greenPale         = RGB 152 251 152
  greenYellow       = RGB 173 255 47
  lawnGreen         = RGB 124 252 0
  limeGreen         = RGB 50  205 50
  mint              = RGB 189 252 201
  olive             = RGB 59  94  43
  oliveDrab         = RGB 107 142 35
  oliveGreenDark    = RGB 85  107 47
  permanentGreen    = RGB 10  201 43
  sapGreen          = RGB 48  128 20
  seaGreen          = RGB 46  139 87
  seaGreenDark      = RGB 143 188 143
  seaGreenMedium    = RGB 60  179 113
  seaGreenLight     = RGB 32  178 170
  springGreen       = RGB 0   255 127
  springGreenMedium = RGB 0   250 154
  terreVerte        = RGB 56  94  15
  viridianLight     = RGB 110 255 112
  yellowGreen       = RGB 154 205 50

  -- Cyans
  aquamarine        = RGB 127 255 212
  aquamarineMedium  = RGB 102 205 170
  cyan              = RGB 0   255 255
  cyanWhite         = RGB 224 255 255
  turquoise         = RGB 64  224 208
  turquoiseDark     = RGB 0   206 209
  turquoiseMedium   = RGB 72  209 204
  turquoisePale     = RGB 175 238 238

  -- Blues
  aliceBlue         = RGB 240 248 255
  blue              = RGB 0   0   255
  blueLight         = RGB 173 216 230
  blueMedium        = RGB 0   0   205
  cadet             = RGB 95  158 160
  cobalt            = RGB 61  89  171
  cornflower        = RGB 100 149 237
  cerulean          = RGB 5   184 204
  dodgerBlue        = RGB 30  144 255
  indigo            = RGB 8   46  84
  manganeseBlue     = RGB 3   168 158
  midnightBlue      = RGB 25  25  112
  navy              = RGB 0   0   128
  peacock           = RGB 51  161 201
  powderBlue        = RGB 176 224 230
  royalBlue         = RGB 65  105 225
  slateBlue         = RGB 106 90  205
  slateBlueDark     = RGB 72  61  139
  slateBlueLight    = RGB 132 112 255
  slateBlueMedium   = RGB 123 104 238
  skyBlue           = RGB 135 206 235
  skyBlueDeep       = RGB 0   191 255
  skyBlueLight      = RGB 135 206 250
  steelBlue         = RGB 70  130 180
  steelBlueLight    = RGB 176 196 222
  turquoiseBlue     = RGB 0   199 140
  ultramarine       = RGB 18  10  143

  -- Magentas
  blueViolet        = RGB 138 43  226
  cobaltVioletDeep  = RGB 145 33  158
  magenta           = RGB 255 0   255
  orchid            = RGB 218 112 214
  orchidDark        = RGB 153 50  204
  orchidMedium      = RGB 186 85  211
  permanentViolet   = RGB 219 38  69
  plum              = RGB 221 160 221
  purple            = RGB 160 32  240
  purpleMedium      = RGB 147 112 219
  ultramarineViolet = RGB 92  36  110
  violet            = RGB 143 94  153
  violetDark        = RGB 148 0   211
  violetRed         = RGB 208 32  144
  violetRedMedium   = RGB 199 21  133
  violetRedPale     = RGB 219 112 147

instance NamedColour (RGB Float) where
  -- Whites
  antiqueWhite      = RGB 0.9804 0.9216 0.8431
  azure             = RGB 0.9412 1.0000 1.0000
  bisque            = RGB 1.0000 0.8941 0.7686
  blanchedAlmond    = RGB 1.0000 0.9216 0.8039
  cornsilk          = RGB 1.0000 0.9725 0.8627
  eggshell          = RGB 0.9900 0.9000 0.7900
  floralWhite       = RGB 1.0000 0.9804 0.9412
  gainsboro         = RGB 0.8627 0.8627 0.8627
  ghostWhite        = RGB 0.9725 0.9725 1.0000
  honeydew          = RGB 0.9412 1.0000 0.9412
  ivory             = RGB 1.0000 1.0000 0.9412
  lavender          = RGB 0.9020 0.9020 0.9804
  lavenderBlush     = RGB 1.0000 0.9412 0.9608
  lemonChiffon      = RGB 1.0000 0.9804 0.8039
  linen             = RGB 0.9804 0.9412 0.9020
  mintCream         = RGB 0.9608 1.0000 0.9804
  mistyRose         = RGB 1.0000 0.8941 0.8824
  moccasin          = RGB 1.0000 0.8941 0.7098
  navajoWhite       = RGB 1.0000 0.8706 0.6784
  oldLace           = RGB 0.9922 0.9608 0.9020
  papayaWhip        = RGB 1.0000 0.9373 0.8353
  peachPuff         = RGB 1.0000 0.8549 0.7255
  seashell          = RGB 1.0000 0.9608 0.9333
  snow              = RGB 1.0000 0.9804 0.9804
  thistle           = RGB 0.8471 0.7490 0.8471
  titaniumWhite     = RGB 0.9900 1.0000 0.9400
  wheat             = RGB 0.9608 0.8706 0.7020
  white             = RGB 1.0000 1.0000 1.0000
  whiteSmoke        = RGB 0.9608 0.9608 0.9608
  zincWhite         = RGB 0.9900 0.9700 1.0000

  -- Greys
  coldGrey          = RGB 0.5000 0.5400 0.5300
  dimGrey           = RGB 0.4118 0.4118 0.4118
  grey              = RGB 0.7529 0.7529 0.7529
  lightGrey         = RGB 0.8275 0.8275 0.8275
  slateGrey         = RGB 0.4392 0.5020 0.5647
  slateGreyDark     = RGB 0.1843 0.3098 0.3098
  slateGreyLight    = RGB 0.4667 0.5333 0.6000
  warmGrey          = RGB 0.5000 0.5000 0.4100

  -- Blacks
  black             = RGB 0.0000 0.0000 0.0000
  ivoryBlack        = RGB 0.1600 0.1400 0.1300
  lampBlack         = RGB 0.1800 0.2800 0.2300

  -- Reds
  alizarinCrimson   = RGB 0.8900 0.1500 0.2100
  brick             = RGB 0.6100 0.4000 0.1200
  cadmiumRedDeep    = RGB 0.8900 0.0900 0.0500
  coral             = RGB 1.0000 0.4980 0.3137
  coralLight        = RGB 0.9412 0.5020 0.5020
  deepPink          = RGB 1.0000 0.0784 0.5765
  englishRed        = RGB 0.8300 0.2400 0.1000
  firebrick         = RGB 0.6980 0.1333 0.1333
  geraniumLake      = RGB 0.8900 0.0700 0.1900
  hotPink           = RGB 1.0000 0.4118 0.7059
  indianRed         = RGB 0.6900 0.0900 0.1200
  lightSalmon       = RGB 1.0000 0.6275 0.4784
  madderLakeDeep    = RGB 0.8900 0.1800 0.1900
  maroon            = RGB 0.6902 0.1882 0.3765
  pink              = RGB 1.0000 0.7529 0.7961
  pinkLight         = RGB 1.0000 0.7137 0.7569
  raspberry         = RGB 0.5300 0.1500 0.3400
  red               = RGB 1.0000 0.0000 0.0000
  roseMadder        = RGB 0.8900 0.2100 0.2200
  salmon            = RGB 0.9804 0.5020 0.4471
  tomato            = RGB 1.0000 0.3882 0.2784
  venetianRed       = RGB 0.8300 0.1000 0.1200

  -- Browns
  beige             = RGB 0.6400 0.5800 0.5000
  brown             = RGB 0.5000 0.1647 0.1647
  brownMadder       = RGB 0.8600 0.1600 0.1600
  brownOchre        = RGB 0.5300 0.2600 0.1200
  burlywood         = RGB 0.8706 0.7216 0.5294
  burntSienna       = RGB 0.5400 0.2100 0.0600
  burntUmber        = RGB 0.5400 0.2000 0.1400
  chocolate         = RGB 0.8235 0.4118 0.1176
  deepOchre         = RGB 0.4500 0.2400 0.1000
  flesh             = RGB 1.0000 0.4900 0.2500
  fleshOchre        = RGB 1.0000 0.3400 0.1300
  goldOchre         = RGB 0.7800 0.4700 0.1500
  greenishUmber     = RGB 1.0000 0.2400 0.0500
  khaki             = RGB 0.9412 0.9020 0.5490
  khakiDark         = RGB 0.7412 0.7176 0.4196
  lightBeige        = RGB 0.9608 0.9608 0.8627
  peru              = RGB 0.8039 0.5216 0.2471
  rosyBrown         = RGB 0.7373 0.5608 0.5608
  rawSienna         = RGB 0.7800 0.3800 0.0800
  rawUmber          = RGB 0.4500 0.2900 0.0700
  sepia             = RGB 0.3700 0.1500 0.0700
  sienna            = RGB 0.6275 0.3216 0.1765
  saddleBrown       = RGB 0.5451 0.2706 0.0745
  sandyBrown        = RGB 0.9569 0.6431 0.3765
  tan               = RGB 0.8235 0.7059 0.5490
  vanDykeBrown      = RGB 0.3700 0.1500 0.0200

  -- Oranges
  cadmiumOrange     = RGB 1.0000 0.3800 0.0100
  cadmiumRedLight   = RGB 1.0000 0.0100 0.0500
  carrot            = RGB 0.9300 0.5700 0.1300
  darkOrange        = RGB 1.0000 0.5490 0.0000
  marsOrange        = RGB 0.5900 0.2700 0.0800
  marsYellow        = RGB 0.8900 0.4400 0.1000
  orange            = RGB 1.0000 0.5000 0.0000
  orangeRed         = RGB 1.0000 0.2706 0.0000
  yellowOchre       = RGB 0.8900 0.5100 0.0900

  -- Yellows
  aureolineYellow   = RGB 1.0000 0.6600 0.1400
  banana            = RGB 0.8900 0.8100 0.3400
  cadmiumLemon      = RGB 1.0000 0.8900 0.0100
  cadmiumYellow     = RGB 1.0000 0.6000 0.0700
  gold              = RGB 1.0000 0.8431 0.0000
  goldenrod         = RGB 0.8549 0.6471 0.1255
  goldenrodDark     = RGB 0.7216 0.5255 0.0431
  goldenrodLight    = RGB 0.9804 0.9804 0.8235
  goldenrodPale     = RGB 0.9333 0.9098 0.6667
  lightGoldenrod    = RGB 0.9333 0.8667 0.5098
  melon             = RGB 0.8900 0.6600 0.4100
  naplesYellowDeep  = RGB 1.0000 0.6600 0.0700
  yellow            = RGB 1.0000 1.0000 0.0000
  yellowLight       = RGB 1.0000 1.0000 0.8784

  -- Greens
  chartreuse        = RGB 0.4980 1.0000 0.0000
  chromeoxideGreen  = RGB 0.4000 0.5000 0.0800
  cinnabarGreen     = RGB 0.3800 0.7000 0.1600
  cobaltGreen       = RGB 0.2400 0.5700 0.2500
  emeraldGreen      = RGB 0.0000 0.7900 0.3400
  forestGreen       = RGB 0.1333 0.5451 0.1333
  green             = RGB 0.0000 1.0000 0.0000
  greenDark         = RGB 0.0000 0.3922 0.0000
  greenPale         = RGB 0.5961 0.9843 0.5961
  greenYellow       = RGB 0.6784 1.0000 0.1843
  lawnGreen         = RGB 0.4863 0.9882 0.0000
  limeGreen         = RGB 0.1961 0.8039 0.1961
  mint              = RGB 0.7400 0.9900 0.7900
  olive             = RGB 0.2300 0.3700 0.1700
  oliveDrab         = RGB 0.4196 0.5569 0.1373
  oliveGreenDark    = RGB 0.3333 0.4196 0.1843
  permanentGreen    = RGB 0.0400 0.7900 0.1700
  sapGreen          = RGB 0.1900 0.5000 0.0800
  seaGreen          = RGB 0.1804 0.5451 0.3412
  seaGreenDark      = RGB 0.5608 0.7373 0.5608
  seaGreenMedium    = RGB 0.2353 0.7020 0.4431
  seaGreenLight     = RGB 0.1255 0.6980 0.6667
  springGreen       = RGB 0.0000 1.0000 0.4980
  springGreenMedium = RGB 0.0000 0.9804 0.6039
  terreVerte        = RGB 0.2200 0.3700 0.0600
  viridianLight     = RGB 0.4300 1.0000 0.4400
  yellowGreen       = RGB 0.6039 0.8039 0.1961

  -- Cyans
  aquamarine        = RGB 0.4980 1.0000 0.8314
  aquamarineMedium  = RGB 0.4000 0.8039 0.6667
  cyan              = RGB 0.0000 1.0000 1.0000
  cyanWhite         = RGB 0.8784 1.0000 1.0000
  turquoise         = RGB 0.2510 0.8784 0.8157
  turquoiseDark     = RGB 0.0000 0.8078 0.8196
  turquoiseMedium   = RGB 0.2824 0.8196 0.8000
  turquoisePale     = RGB 0.6863 0.9333 0.9333

  -- Blues
  aliceBlue         = RGB 0.9412 0.9725 1.0000
  blue              = RGB 0.0000 0.0000 1.0000
  blueLight         = RGB 0.6784 0.8471 0.9020
  blueMedium        = RGB 0.0000 0.0000 0.8039
  cadet             = RGB 0.3725 0.6196 0.6275
  cobalt            = RGB 0.2400 0.3500 0.6700
  cornflower        = RGB 0.3922 0.5843 0.9294
  cerulean          = RGB 0.0200 0.7200 0.8000
  dodgerBlue        = RGB 0.1176 0.5647 1.0000
  indigo            = RGB 0.0300 0.1800 0.3300
  manganeseBlue     = RGB 0.0100 0.6600 0.6200
  midnightBlue      = RGB 0.0980 0.0980 0.4392
  navy              = RGB 0.0000 0.0000 0.5020
  peacock           = RGB 0.2000 0.6300 0.7900
  powderBlue        = RGB 0.6902 0.8784 0.9020
  royalBlue         = RGB 0.2549 0.4118 0.8824
  slateBlue         = RGB 0.4157 0.3529 0.8039
  slateBlueDark     = RGB 0.2824 0.2392 0.5451
  slateBlueLight    = RGB 0.5176 0.4392 1.0000
  slateBlueMedium   = RGB 0.4824 0.4078 0.9333
  skyBlue           = RGB 0.5294 0.8078 0.9216
  skyBlueDeep       = RGB 0.0000 0.7490 1.0000
  skyBlueLight      = RGB 0.5294 0.8078 0.9804
  steelBlue         = RGB 0.2745 0.5098 0.7059
  steelBlueLight    = RGB 0.6902 0.7686 0.8706
  turquoiseBlue     = RGB 0.0000 0.7800 0.5500
  ultramarine       = RGB 0.0700 0.0400 0.5600

  -- Magentas
  blueViolet        = RGB 0.5412 0.1686 0.8863
  cobaltVioletDeep  = RGB 0.5700 0.1300 0.6200
  magenta           = RGB 1.0000 0.0000 1.0000
  orchid            = RGB 0.8549 0.4392 0.8392
  orchidDark        = RGB 0.6000 0.1961 0.8000
  orchidMedium      = RGB 0.7294 0.3333 0.8275
  permanentViolet   = RGB 0.8600 0.1500 0.2700
  plum              = RGB 0.8667 0.6275 0.8667
  purple            = RGB 0.6275 0.1255 0.9412
  purpleMedium      = RGB 0.5765 0.4392 0.8588
  ultramarineViolet = RGB 0.3600 0.1400 0.4300
  violet            = RGB 0.5600 0.3700 0.6000
  violetDark        = RGB 0.5804 0.0000 0.8275
  violetRed         = RGB 0.8157 0.1255 0.5647
  violetRedMedium   = RGB 0.7804 0.0824 0.5216
  violetRedPale     = RGB 0.8588 0.4392 0.5765

