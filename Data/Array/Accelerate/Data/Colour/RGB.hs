{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  blend,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.Names

import Data.Typeable


-- | An RGB colour value
--
type Colour = RGB Float

-- | Construct an RGB colour from individual channel components. The components
-- will be clamped to the range [0..1].
--
rgb :: Exp Float -> Exp Float -> Exp Float -> Exp Colour
rgb r g b
  = clamp
  $ lift (RGB r g b)


-- | Construct a colour from 8-bit-per-channel colour components.
--
rgb8 :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Colour
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
    c x = 0 `max` x `min` 1


-- | Blend two colours in the given proportions
--
blend :: Exp Float      -- ^ proportion of first colour
      -> Exp Float      -- ^ proportion of second colour
      -> Exp Colour     -- ^ first colour
      -> Exp Colour     -- ^ second colour
      -> Exp Colour
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


-- Accelerate bits
-- ---------------

-- RGB colour space
--
data RGB a = RGB a a a
  deriving (Show, Eq, Functor, Typeable)

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

{--
instance Num a => Num (RGB a) where
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


instance (Elt a, IsNum a) => Num (Exp (RGB a)) where
  (+)           = lift2 ((+) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (-)           = lift2 ((-) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (*)           = lift2 ((*) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  abs           = lift1 (abs :: RGB (Exp a) -> RGB (Exp a))
  signum        = lift1 (signum :: RGB (Exp a) -> RGB (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                  in lift $ RGB f f f
--}


-- Named colours
-- -------------

instance NamedColour (RGB Word8) where
  -- Whites
  antiqueWhite      = constant $ RGB 250 235 215
  azure             = constant $ RGB 240 255 255
  bisque            = constant $ RGB 255 228 196
  blanchedAlmond    = constant $ RGB 255 235 205
  cornsilk          = constant $ RGB 255 248 220
  eggshell          = constant $ RGB 252 230 201
  floralWhite       = constant $ RGB 255 250 240
  gainsboro         = constant $ RGB 220 220 220
  ghostWhite        = constant $ RGB 248 248 255
  honeydew          = constant $ RGB 240 255 240
  ivory             = constant $ RGB 255 255 240
  lavender          = constant $ RGB 230 230 250
  lavenderBlush     = constant $ RGB 255 240 245
  lemonChiffon      = constant $ RGB 255 250 205
  linen             = constant $ RGB 250 240 230
  mintCream         = constant $ RGB 245 255 250
  mistyRose         = constant $ RGB 255 228 225
  moccasin          = constant $ RGB 255 228 181
  navajoWhite       = constant $ RGB 255 222 173
  oldLace           = constant $ RGB 253 245 230
  papayaWhip        = constant $ RGB 255 239 213
  peachPuff         = constant $ RGB 255 218 185
  seashell          = constant $ RGB 255 245 238
  snow              = constant $ RGB 255 250 250
  thistle           = constant $ RGB 216 191 216
  titaniumWhite     = constant $ RGB 252 255 240
  wheat             = constant $ RGB 245 222 179
  white             = constant $ RGB 255 255 255
  whiteSmoke        = constant $ RGB 245 245 245
  zincWhite         = constant $ RGB 253 248 255

  -- Greys
  coldGrey          = constant $ RGB 128 138 135
  dimGrey           = constant $ RGB 105 105 105
  grey              = constant $ RGB 192 192 192
  lightGrey         = constant $ RGB 211 211 211
  slateGrey         = constant $ RGB 112 128 144
  slateGreyDark     = constant $ RGB 47  79  79
  slateGreyLight    = constant $ RGB 119 136 153
  warmGrey          = constant $ RGB 128 128 105

  -- Blacks
  black             = constant $ RGB 0   0   0
  ivoryBlack        = constant $ RGB 41  36  33
  lampBlack         = constant $ RGB 46  71  59

  -- Reds
  alizarinCrimson   = constant $ RGB 227 38  54
  brick             = constant $ RGB 156 102 31
  cadmiumRedDeep    = constant $ RGB 227 23  13
  coral             = constant $ RGB 255 127 80
  coralLight        = constant $ RGB 240 128 128
  deepPink          = constant $ RGB 255 20  147
  englishRed        = constant $ RGB 212 61  26
  firebrick         = constant $ RGB 178 34  34
  geraniumLake      = constant $ RGB 227 18  48
  hotPink           = constant $ RGB 255 105 180
  indianRed         = constant $ RGB 176 23  31
  lightSalmon       = constant $ RGB 255 160 122
  madderLakeDeep    = constant $ RGB 227 46  48
  maroon            = constant $ RGB 176 48  96
  pink              = constant $ RGB 255 192 203
  pinkLight         = constant $ RGB 255 182 193
  raspberry         = constant $ RGB 135 38  87
  red               = constant $ RGB 255 0   0
  roseMadder        = constant $ RGB 227 54  56
  salmon            = constant $ RGB 250 128 114
  tomato            = constant $ RGB 255 99  71
  venetianRed       = constant $ RGB 212 26  31

  -- Browns
  beige             = constant $ RGB 163 148 128
  brown             = constant $ RGB 128 42  42
  brownMadder       = constant $ RGB 219 41  41
  brownOchre        = constant $ RGB 135 66  31
  burlywood         = constant $ RGB 222 184 135
  burntSienna       = constant $ RGB 138 54  15
  burntUmber        = constant $ RGB 138 51  36
  chocolate         = constant $ RGB 210 105 30
  deepOchre         = constant $ RGB 115 61  26
  flesh             = constant $ RGB 255 125 64
  fleshOchre        = constant $ RGB 255 87  33
  goldOchre         = constant $ RGB 199 120 38
  greenishUmber     = constant $ RGB 255 61  13
  khaki             = constant $ RGB 240 230 140
  khakiDark         = constant $ RGB 189 183 107
  lightBeige        = constant $ RGB 245 245 220
  peru              = constant $ RGB 205 133 63
  rosyBrown         = constant $ RGB 188 143 143
  rawSienna         = constant $ RGB 199 97  20
  rawUmber          = constant $ RGB 115 74  18
  sepia             = constant $ RGB 94  38  18
  sienna            = constant $ RGB 160 82  45
  saddleBrown       = constant $ RGB 139 69  19
  sandyBrown        = constant $ RGB 244 164 96
  tan               = constant $ RGB 210 180 140
  vanDykeBrown      = constant $ RGB 94  38  5

  -- Oranges
  cadmiumOrange     = constant $ RGB 255 97  3
  cadmiumRedLight   = constant $ RGB 255 3   13
  carrot            = constant $ RGB 237 145 33
  darkOrange        = constant $ RGB 255 140 0
  marsOrange        = constant $ RGB 150 69  20
  marsYellow        = constant $ RGB 227 112 26
  orange            = constant $ RGB 255 128 0
  orangeRed         = constant $ RGB 255 69  0
  yellowOchre       = constant $ RGB 227 130 23

  -- Yellows
  aureolineYellow   = constant $ RGB 255 168 36
  banana            = constant $ RGB 227 207 87
  cadmiumLemon      = constant $ RGB 255 227 3
  cadmiumYellow     = constant $ RGB 255 153 18
  gold              = constant $ RGB 255 215 0
  goldenrod         = constant $ RGB 218 165 32
  goldenrodDark     = constant $ RGB 184 134 11
  goldenrodLight    = constant $ RGB 250 250 210
  goldenrodPale     = constant $ RGB 238 232 170
  lightGoldenrod    = constant $ RGB 238 221 130
  melon             = constant $ RGB 227 168 105
  naplesyellowdeep  = constant $ RGB 255 168 18
  yellow            = constant $ RGB 255 255 0
  yellowLight       = constant $ RGB 255 255 224

  -- Greens
  chartreuse        = constant $ RGB 127 255 0
  chromeoxidegreen  = constant $ RGB 102 128 20
  cinnabarGreen     = constant $ RGB 97  179 41
  cobaltGreen       = constant $ RGB 61  145 64
  emeraldGreen      = constant $ RGB 0   201 87
  forestGreen       = constant $ RGB 34  139 34
  green             = constant $ RGB 0   255 0
  greenDark         = constant $ RGB 0   100 0
  greenPale         = constant $ RGB 152 251 152
  greenYellow       = constant $ RGB 173 255 47
  lawnGreen         = constant $ RGB 124 252 0
  limeGreen         = constant $ RGB 50  205 50
  mint              = constant $ RGB 189 252 201
  olive             = constant $ RGB 59  94  43
  oliveDrab         = constant $ RGB 107 142 35
  oliveGreenDark    = constant $ RGB 85  107 47
  permanentGreen    = constant $ RGB 10  201 43
  sapGreen          = constant $ RGB 48  128 20
  seaGreen          = constant $ RGB 46  139 87
  seaGreenDark      = constant $ RGB 143 188 143
  seaGreenMedium    = constant $ RGB 60  179 113
  seaGreenLight     = constant $ RGB 32  178 170
  springGreen       = constant $ RGB 0   255 127
  springGreenmedium = constant $ RGB 0   250 154
  terreVerte        = constant $ RGB 56  94  15
  viridianLight     = constant $ RGB 110 255 112
  yellowGreen       = constant $ RGB 154 205 50

  -- Cyans
  aquamarine        = constant $ RGB 127 255 212
  aquamarinemedium  = constant $ RGB 102 205 170
  cyan              = constant $ RGB 0   255 255
  cyanWhite         = constant $ RGB 224 255 255
  turquoise         = constant $ RGB 64  224 208
  turquoiseDark     = constant $ RGB 0   206 209
  turquoiseMedium   = constant $ RGB 72  209 204
  turquoisePale     = constant $ RGB 175 238 238

  -- Blues
  aliceBlue         = constant $ RGB 240 248 255
  blue              = constant $ RGB 0   0   255
  blueLight         = constant $ RGB 173 216 230
  blueMedium        = constant $ RGB 0   0   205
  cadet             = constant $ RGB 95  158 160
  cobalt            = constant $ RGB 61  89  171
  cornflower        = constant $ RGB 100 149 237
  cerulean          = constant $ RGB 5   184 204
  dodgerBlue        = constant $ RGB 30  144 255
  indigo            = constant $ RGB 8   46  84
  manganeseBlue     = constant $ RGB 3   168 158
  midnightBlue      = constant $ RGB 25  25  112
  navy              = constant $ RGB 0   0   128
  peacock           = constant $ RGB 51  161 201
  powderBlue        = constant $ RGB 176 224 230
  royalBlue         = constant $ RGB 65  105 225
  slateBlue         = constant $ RGB 106 90  205
  slateBlueDark     = constant $ RGB 72  61  139
  slateBlueLight    = constant $ RGB 132 112 255
  slateBlueMedium   = constant $ RGB 123 104 238
  skyBlue           = constant $ RGB 135 206 235
  skyBlueDeep       = constant $ RGB 0   191 255
  skyBlueLight      = constant $ RGB 135 206 250
  steelBlue         = constant $ RGB 70  130 180
  steelBlueLight    = constant $ RGB 176 196 222
  turquoiseBlue     = constant $ RGB 0   199 140
  ultramarine       = constant $ RGB 18  10  143

  -- Magentas
  blueViolet        = constant $ RGB 138 43  226
  cobaltVioletdeep  = constant $ RGB 145 33  158
  magenta           = constant $ RGB 255 0   255
  orchid            = constant $ RGB 218 112 214
  orchidDark        = constant $ RGB 153 50  204
  orchidMedium      = constant $ RGB 186 85  211
  permanentViolet   = constant $ RGB 219 38  69
  plum              = constant $ RGB 221 160 221
  purple            = constant $ RGB 160 32  240
  purpleMedium      = constant $ RGB 147 112 219
  ultramarineViolet = constant $ RGB 92  36  110
  violet            = constant $ RGB 143 94  153
  violetDark        = constant $ RGB 148 0   211
  violetRed         = constant $ RGB 208 32  144
  violetRedmedium   = constant $ RGB 199 21  133
  violetRedPale     = constant $ RGB 219 112 147

instance NamedColour (RGB Float) where
  -- Whites
  antiqueWhite      = constant $ RGB 0.9804 0.9216 0.8431
  azure             = constant $ RGB 0.9412 1.0000 1.0000
  bisque            = constant $ RGB 1.0000 0.8941 0.7686
  blanchedAlmond    = constant $ RGB 1.0000 0.9216 0.8039
  cornsilk          = constant $ RGB 1.0000 0.9725 0.8627
  eggshell          = constant $ RGB 0.9900 0.9000 0.7900
  floralWhite       = constant $ RGB 1.0000 0.9804 0.9412
  gainsboro         = constant $ RGB 0.8627 0.8627 0.8627
  ghostWhite        = constant $ RGB 0.9725 0.9725 1.0000
  honeydew          = constant $ RGB 0.9412 1.0000 0.9412
  ivory             = constant $ RGB 1.0000 1.0000 0.9412
  lavender          = constant $ RGB 0.9020 0.9020 0.9804
  lavenderBlush     = constant $ RGB 1.0000 0.9412 0.9608
  lemonChiffon      = constant $ RGB 1.0000 0.9804 0.8039
  linen             = constant $ RGB 0.9804 0.9412 0.9020
  mintCream         = constant $ RGB 0.9608 1.0000 0.9804
  mistyRose         = constant $ RGB 1.0000 0.8941 0.8824
  moccasin          = constant $ RGB 1.0000 0.8941 0.7098
  navajoWhite       = constant $ RGB 1.0000 0.8706 0.6784
  oldLace           = constant $ RGB 0.9922 0.9608 0.9020
  papayaWhip        = constant $ RGB 1.0000 0.9373 0.8353
  peachPuff         = constant $ RGB 1.0000 0.8549 0.7255
  seashell          = constant $ RGB 1.0000 0.9608 0.9333
  snow              = constant $ RGB 1.0000 0.9804 0.9804
  thistle           = constant $ RGB 0.8471 0.7490 0.8471
  titaniumWhite     = constant $ RGB 0.9900 1.0000 0.9400
  wheat             = constant $ RGB 0.9608 0.8706 0.7020
  white             = constant $ RGB 1.0000 1.0000 1.0000
  whiteSmoke        = constant $ RGB 0.9608 0.9608 0.9608
  zincWhite         = constant $ RGB 0.9900 0.9700 1.0000

  -- Greys
  coldGrey          = constant $ RGB 0.5000 0.5400 0.5300
  dimGrey           = constant $ RGB 0.4118 0.4118 0.4118
  grey              = constant $ RGB 0.7529 0.7529 0.7529
  lightGrey         = constant $ RGB 0.8275 0.8275 0.8275
  slateGrey         = constant $ RGB 0.4392 0.5020 0.5647
  slateGreyDark     = constant $ RGB 0.1843 0.3098 0.3098
  slateGreyLight    = constant $ RGB 0.4667 0.5333 0.6000
  warmGrey          = constant $ RGB 0.5000 0.5000 0.4100

  -- Blacks
  black             = constant $ RGB 0.0000 0.0000 0.0000
  ivoryBlack        = constant $ RGB 0.1600 0.1400 0.1300
  lampBlack         = constant $ RGB 0.1800 0.2800 0.2300

  -- Reds
  alizarinCrimson   = constant $ RGB 0.8900 0.1500 0.2100
  brick             = constant $ RGB 0.6100 0.4000 0.1200
  cadmiumRedDeep    = constant $ RGB 0.8900 0.0900 0.0500
  coral             = constant $ RGB 1.0000 0.4980 0.3137
  coralLight        = constant $ RGB 0.9412 0.5020 0.5020
  deepPink          = constant $ RGB 1.0000 0.0784 0.5765
  englishRed        = constant $ RGB 0.8300 0.2400 0.1000
  firebrick         = constant $ RGB 0.6980 0.1333 0.1333
  geraniumLake      = constant $ RGB 0.8900 0.0700 0.1900
  hotPink           = constant $ RGB 1.0000 0.4118 0.7059
  indianRed         = constant $ RGB 0.6900 0.0900 0.1200
  lightSalmon       = constant $ RGB 1.0000 0.6275 0.4784
  madderLakeDeep    = constant $ RGB 0.8900 0.1800 0.1900
  maroon            = constant $ RGB 0.6902 0.1882 0.3765
  pink              = constant $ RGB 1.0000 0.7529 0.7961
  pinkLight         = constant $ RGB 1.0000 0.7137 0.7569
  raspberry         = constant $ RGB 0.5300 0.1500 0.3400
  red               = constant $ RGB 1.0000 0.0000 0.0000
  roseMadder        = constant $ RGB 0.8900 0.2100 0.2200
  salmon            = constant $ RGB 0.9804 0.5020 0.4471
  tomato            = constant $ RGB 1.0000 0.3882 0.2784
  venetianRed       = constant $ RGB 0.8300 0.1000 0.1200

  -- Browns
  beige             = constant $ RGB 0.6400 0.5800 0.5000
  brown             = constant $ RGB 0.5000 0.1647 0.1647
  brownMadder       = constant $ RGB 0.8600 0.1600 0.1600
  brownOchre        = constant $ RGB 0.5300 0.2600 0.1200
  burlywood         = constant $ RGB 0.8706 0.7216 0.5294
  burntSienna       = constant $ RGB 0.5400 0.2100 0.0600
  burntUmber        = constant $ RGB 0.5400 0.2000 0.1400
  chocolate         = constant $ RGB 0.8235 0.4118 0.1176
  deepOchre         = constant $ RGB 0.4500 0.2400 0.1000
  flesh             = constant $ RGB 1.0000 0.4900 0.2500
  fleshOchre        = constant $ RGB 1.0000 0.3400 0.1300
  goldOchre         = constant $ RGB 0.7800 0.4700 0.1500
  greenishUmber     = constant $ RGB 1.0000 0.2400 0.0500
  khaki             = constant $ RGB 0.9412 0.9020 0.5490
  khakiDark         = constant $ RGB 0.7412 0.7176 0.4196
  lightBeige        = constant $ RGB 0.9608 0.9608 0.8627
  peru              = constant $ RGB 0.8039 0.5216 0.2471
  rosyBrown         = constant $ RGB 0.7373 0.5608 0.5608
  rawSienna         = constant $ RGB 0.7800 0.3800 0.0800
  rawUmber          = constant $ RGB 0.4500 0.2900 0.0700
  sepia             = constant $ RGB 0.3700 0.1500 0.0700
  sienna            = constant $ RGB 0.6275 0.3216 0.1765
  saddleBrown       = constant $ RGB 0.5451 0.2706 0.0745
  sandyBrown        = constant $ RGB 0.9569 0.6431 0.3765
  tan               = constant $ RGB 0.8235 0.7059 0.5490
  vanDykeBrown      = constant $ RGB 0.3700 0.1500 0.0200

  -- Oranges
  cadmiumOrange     = constant $ RGB 1.0000 0.3800 0.0100
  cadmiumRedLight   = constant $ RGB 1.0000 0.0100 0.0500
  carrot            = constant $ RGB 0.9300 0.5700 0.1300
  darkOrange        = constant $ RGB 1.0000 0.5490 0.0000
  marsOrange        = constant $ RGB 0.5900 0.2700 0.0800
  marsYellow        = constant $ RGB 0.8900 0.4400 0.1000
  orange            = constant $ RGB 1.0000 0.5000 0.0000
  orangeRed         = constant $ RGB 1.0000 0.2706 0.0000
  yellowOchre       = constant $ RGB 0.8900 0.5100 0.0900

  -- Yellows
  aureolineYellow   = constant $ RGB 1.0000 0.6600 0.1400
  banana            = constant $ RGB 0.8900 0.8100 0.3400
  cadmiumLemon      = constant $ RGB 1.0000 0.8900 0.0100
  cadmiumYellow     = constant $ RGB 1.0000 0.6000 0.0700
  gold              = constant $ RGB 1.0000 0.8431 0.0000
  goldenrod         = constant $ RGB 0.8549 0.6471 0.1255
  goldenrodDark     = constant $ RGB 0.7216 0.5255 0.0431
  goldenrodLight    = constant $ RGB 0.9804 0.9804 0.8235
  goldenrodPale     = constant $ RGB 0.9333 0.9098 0.6667
  lightGoldenrod    = constant $ RGB 0.9333 0.8667 0.5098
  melon             = constant $ RGB 0.8900 0.6600 0.4100
  naplesyellowdeep  = constant $ RGB 1.0000 0.6600 0.0700
  yellow            = constant $ RGB 1.0000 1.0000 0.0000
  yellowLight       = constant $ RGB 1.0000 1.0000 0.8784

  -- Greens
  chartreuse        = constant $ RGB 0.4980 1.0000 0.0000
  chromeoxidegreen  = constant $ RGB 0.4000 0.5000 0.0800
  cinnabarGreen     = constant $ RGB 0.3800 0.7000 0.1600
  cobaltGreen       = constant $ RGB 0.2400 0.5700 0.2500
  emeraldGreen      = constant $ RGB 0.0000 0.7900 0.3400
  forestGreen       = constant $ RGB 0.1333 0.5451 0.1333
  green             = constant $ RGB 0.0000 1.0000 0.0000
  greenDark         = constant $ RGB 0.0000 0.3922 0.0000
  greenPale         = constant $ RGB 0.5961 0.9843 0.5961
  greenYellow       = constant $ RGB 0.6784 1.0000 0.1843
  lawnGreen         = constant $ RGB 0.4863 0.9882 0.0000
  limeGreen         = constant $ RGB 0.1961 0.8039 0.1961
  mint              = constant $ RGB 0.7400 0.9900 0.7900
  olive             = constant $ RGB 0.2300 0.3700 0.1700
  oliveDrab         = constant $ RGB 0.4196 0.5569 0.1373
  oliveGreenDark    = constant $ RGB 0.3333 0.4196 0.1843
  permanentGreen    = constant $ RGB 0.0400 0.7900 0.1700
  sapGreen          = constant $ RGB 0.1900 0.5000 0.0800
  seaGreen          = constant $ RGB 0.1804 0.5451 0.3412
  seaGreenDark      = constant $ RGB 0.5608 0.7373 0.5608
  seaGreenMedium    = constant $ RGB 0.2353 0.7020 0.4431
  seaGreenLight     = constant $ RGB 0.1255 0.6980 0.6667
  springGreen       = constant $ RGB 0.0000 1.0000 0.4980
  springGreenmedium = constant $ RGB 0.0000 0.9804 0.6039
  terreVerte        = constant $ RGB 0.2200 0.3700 0.0600
  viridianLight     = constant $ RGB 0.4300 1.0000 0.4400
  yellowGreen       = constant $ RGB 0.6039 0.8039 0.1961

  -- Cyans
  aquamarine        = constant $ RGB 0.4980 1.0000 0.8314
  aquamarinemedium  = constant $ RGB 0.4000 0.8039 0.6667
  cyan              = constant $ RGB 0.0000 1.0000 1.0000
  cyanWhite         = constant $ RGB 0.8784 1.0000 1.0000
  turquoise         = constant $ RGB 0.2510 0.8784 0.8157
  turquoiseDark     = constant $ RGB 0.0000 0.8078 0.8196
  turquoiseMedium   = constant $ RGB 0.2824 0.8196 0.8000
  turquoisePale     = constant $ RGB 0.6863 0.9333 0.9333

  -- Blues
  aliceBlue         = constant $ RGB 0.9412 0.9725 1.0000
  blue              = constant $ RGB 0.0000 0.0000 1.0000
  blueLight         = constant $ RGB 0.6784 0.8471 0.9020
  blueMedium        = constant $ RGB 0.0000 0.0000 0.8039
  cadet             = constant $ RGB 0.3725 0.6196 0.6275
  cobalt            = constant $ RGB 0.2400 0.3500 0.6700
  cornflower        = constant $ RGB 0.3922 0.5843 0.9294
  cerulean          = constant $ RGB 0.0200 0.7200 0.8000
  dodgerBlue        = constant $ RGB 0.1176 0.5647 1.0000
  indigo            = constant $ RGB 0.0300 0.1800 0.3300
  manganeseBlue     = constant $ RGB 0.0100 0.6600 0.6200
  midnightBlue      = constant $ RGB 0.0980 0.0980 0.4392
  navy              = constant $ RGB 0.0000 0.0000 0.5020
  peacock           = constant $ RGB 0.2000 0.6300 0.7900
  powderBlue        = constant $ RGB 0.6902 0.8784 0.9020
  royalBlue         = constant $ RGB 0.2549 0.4118 0.8824
  slateBlue         = constant $ RGB 0.4157 0.3529 0.8039
  slateBlueDark     = constant $ RGB 0.2824 0.2392 0.5451
  slateBlueLight    = constant $ RGB 0.5176 0.4392 1.0000
  slateBlueMedium   = constant $ RGB 0.4824 0.4078 0.9333
  skyBlue           = constant $ RGB 0.5294 0.8078 0.9216
  skyBlueDeep       = constant $ RGB 0.0000 0.7490 1.0000
  skyBlueLight      = constant $ RGB 0.5294 0.8078 0.9804
  steelBlue         = constant $ RGB 0.2745 0.5098 0.7059
  steelBlueLight    = constant $ RGB 0.6902 0.7686 0.8706
  turquoiseBlue     = constant $ RGB 0.0000 0.7800 0.5500
  ultramarine       = constant $ RGB 0.0700 0.0400 0.5600

  -- Magentas
  blueViolet        = constant $ RGB 0.5412 0.1686 0.8863
  cobaltVioletdeep  = constant $ RGB 0.5700 0.1300 0.6200
  magenta           = constant $ RGB 1.0000 0.0000 1.0000
  orchid            = constant $ RGB 0.8549 0.4392 0.8392
  orchidDark        = constant $ RGB 0.6000 0.1961 0.8000
  orchidMedium      = constant $ RGB 0.7294 0.3333 0.8275
  permanentViolet   = constant $ RGB 0.8600 0.1500 0.2700
  plum              = constant $ RGB 0.8667 0.6275 0.8667
  purple            = constant $ RGB 0.6275 0.1255 0.9412
  purpleMedium      = constant $ RGB 0.5765 0.4392 0.8588
  ultramarineViolet = constant $ RGB 0.3600 0.1400 0.4300
  violet            = constant $ RGB 0.5600 0.3700 0.6000
  violetDark        = constant $ RGB 0.5804 0.0000 0.8275
  violetRed         = constant $ RGB 0.8157 0.1255 0.5647
  violetRedmedium   = constant $ RGB 0.7804 0.0824 0.5216
  violetRedPale     = constant $ RGB 0.8588 0.4392 0.5765

