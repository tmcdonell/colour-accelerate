{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.ARGB
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- RGBA quadruples for an unspecified colour space
--

module Data.Array.Accelerate.Data.Colour.RGBA (

  Colour,
  RGBA(..),

  rgba, rgba8,
  blend,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.Names

import Data.Typeable


-- | An RGBA colour value.
--
type Colour = RGBA Float


-- | Construct an RGB colour from individual channel components. The components
-- will be clamped to the rang [0..1].
--
rgba :: Exp Float     -- ^ red component
     -> Exp Float     -- ^ green component
     -> Exp Float     -- ^ blue component
     -> Exp Float     -- ^ alpha component
     -> Exp Colour
rgba r g b a
  = clamp
  $ lift (RGBA r g b a)


-- | Construct a colour from 8-bits-per-channel colour components.
--
rgba8
    :: Exp Word8      -- ^ red component
    -> Exp Word8      -- ^ green component
    -> Exp Word8      -- ^ blue component
    -> Exp Word8      -- ^ alpha component
    -> Exp Colour
rgba8 r g b a
  = lift
  $ RGBA (A.fromIntegral r / 255)
         (A.fromIntegral g / 255)
         (A.fromIntegral b / 255)
         (A.fromIntegral a / 255)


-- | Clamp each component to the range [0..1]
--
clamp :: Exp Colour -> Exp Colour
clamp = lift1 (fmap c :: RGBA (Exp Float) -> RGBA (Exp Float))
  where
    c x = 0 `max` x `min` 1


-- | Blend two colours in the given proportions
--
blend :: Exp Float      -- ^ Proportion of first colour
      -> Exp Float      -- ^ Proportion of second colour
      -> Exp Colour     -- ^ First colour
      -> Exp Colour     -- ^ Second colour
      -> Exp Colour     -- ^ Resulting colour
blend m1 m2 c1 c2 =
  let
      RGBA r1 g1 b1 a1  = unlift c1
      RGBA r2 g2 b2 a2  = unlift c2

      -- Normalise mixing proportions to ratios.
      m12 = m1 + m2
      m1' = m1 / m12
      m2' = m2 / m12

      -- Colour components should be added via sum of squares, otherwise the
      -- result will be too dark.
      r1s = r1 * r1;    r2s = r2 * r2
      g1s = g1 * g1;    g2s = g2 * g2
      b1s = b1 * b1;    b2s = b2 * b2
  in
  rgba (sqrt (m1' * r1s + m2' * r2s))
       (sqrt (m1' * g1s + m2' * g2s))
       (sqrt (m1' * b1s + m2' * b2s))
       ((m1 * a1 + m2 * a2) / m12)


-- Accelerate bits
-- ---------------

-- | An RGBA colour value to hold the colour components. All components lie in
-- the range [0..1).
--
-- We need to parameterise by a type so that we can have both Exp (RGBA a) and
-- RGBA (Exp a).
--
data RGBA a = RGBA a a a a
  deriving (Show, Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 4-tuple
--
type instance EltRepr (RGBA a) = EltRepr (a, a, a, a)

instance Elt a => Elt (RGBA a) where
  eltType (_ :: RGBA a)         = eltType (undefined :: (a,a,a,a))
  toElt c                       = let (r,g,b,a) = toElt c in RGBA r g b a
  fromElt (RGBA r g b a)        = fromElt (r,g,b,a)

instance Elt a => IsProduct Elt (RGBA a) where
  type ProdRepr (RGBA a)         = (((((),a), a), a), a)
  fromProd _ (RGBA r g b a)      = (((((), r), g), b), a)
  toProd _ (((((),r),g),b),a)    = RGBA r g b a
  prod cst _                     = prod cst (undefined :: (a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGBA a) where
  type Plain (RGBA a)   = RGBA (Plain a)
  lift (RGBA r g b a)   = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g
                                               `SnocTup` lift b `SnocTup` lift a

instance Elt a => Unlift Exp (RGBA (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` c
                      g = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      b = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      a = Exp $ ZeroTupIdx `Prj` c
                  in RGBA r g b a

{--
instance Num a => Num (RGBA a) where
  (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1

  (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1

  (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1

  abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1

  signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1

  fromInteger i
        = let f = fromInteger i
          in  RGBA f f f 1

instance (Elt a, IsNum a) => Num (Exp (RGBA a)) where
  (+)           = lift2 ((+) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (-)           = lift2 ((-) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (*)           = lift2 ((*) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  abs           = lift1 (abs :: RGBA (Exp a) -> RGBA (Exp a))
  signum        = lift1 (signum :: RGBA (Exp a) -> RGBA (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                      a = constant 1
                  in lift $ RGBA f f f a
--}


-- Named colours
-- -------------

instance NamedColour (RGBA Word8) where
  -- Whites
  antiqueWhite      = constant $ RGBA 250 235 215 255
  azure             = constant $ RGBA 240 255 255 255
  bisque            = constant $ RGBA 255 228 196 255
  blanchedAlmond    = constant $ RGBA 255 235 205 255
  cornsilk          = constant $ RGBA 255 248 220 255
  eggshell          = constant $ RGBA 252 230 201 255
  floralWhite       = constant $ RGBA 255 250 240 255
  gainsboro         = constant $ RGBA 220 220 220 255
  ghostWhite        = constant $ RGBA 248 248 255 255
  honeydew          = constant $ RGBA 240 255 240 255
  ivory             = constant $ RGBA 255 255 240 255
  lavender          = constant $ RGBA 230 230 250 255
  lavenderBlush     = constant $ RGBA 255 240 245 255
  lemonChiffon      = constant $ RGBA 255 250 205 255
  linen             = constant $ RGBA 250 240 230 255
  mintCream         = constant $ RGBA 245 255 250 255
  mistyRose         = constant $ RGBA 255 228 225 255
  moccasin          = constant $ RGBA 255 228 181 255
  navajoWhite       = constant $ RGBA 255 222 173 255
  oldLace           = constant $ RGBA 253 245 230 255
  papayaWhip        = constant $ RGBA 255 239 213 255
  peachPuff         = constant $ RGBA 255 218 185 255
  seashell          = constant $ RGBA 255 245 238 255
  snow              = constant $ RGBA 255 250 250 255
  thistle           = constant $ RGBA 216 191 216 255
  titaniumWhite     = constant $ RGBA 252 255 240 255
  wheat             = constant $ RGBA 245 222 179 255
  white             = constant $ RGBA 255 255 255 255
  whiteSmoke        = constant $ RGBA 245 245 245 255
  zincWhite         = constant $ RGBA 253 248 255 255

  -- Greys
  coldGrey          = constant $ RGBA 128 138 135 255
  dimGrey           = constant $ RGBA 105 105 105 255
  grey              = constant $ RGBA 192 192 192 255
  lightGrey         = constant $ RGBA 211 211 211 255
  slateGrey         = constant $ RGBA 112 128 144 255
  slateGreyDark     = constant $ RGBA 47  79  79  255
  slateGreyLight    = constant $ RGBA 119 136 153 255
  warmGrey          = constant $ RGBA 128 128 105 255

  -- Blacks
  black             = constant $ RGBA 0   0   0   255
  ivoryBlack        = constant $ RGBA 41  36  33  255
  lampBlack         = constant $ RGBA 46  71  59  255

  -- Reds
  alizarinCrimson   = constant $ RGBA 227 38  54  255
  brick             = constant $ RGBA 156 102 31  255
  cadmiumRedDeep    = constant $ RGBA 227 23  13  255
  coral             = constant $ RGBA 255 127 80  255
  coralLight        = constant $ RGBA 240 128 128 255
  deepPink          = constant $ RGBA 255 20  147 255
  englishRed        = constant $ RGBA 212 61  26  255
  firebrick         = constant $ RGBA 178 34  34  255
  geraniumLake      = constant $ RGBA 227 18  48  255
  hotPink           = constant $ RGBA 255 105 180 255
  indianRed         = constant $ RGBA 176 23  31  255
  lightSalmon       = constant $ RGBA 255 160 122 255
  madderLakeDeep    = constant $ RGBA 227 46  48  255
  maroon            = constant $ RGBA 176 48  96  255
  pink              = constant $ RGBA 255 192 203 255
  pinkLight         = constant $ RGBA 255 182 193 255
  raspberry         = constant $ RGBA 135 38  87  255
  red               = constant $ RGBA 255 0   0   255
  roseMadder        = constant $ RGBA 227 54  56  255
  salmon            = constant $ RGBA 250 128 114 255
  tomato            = constant $ RGBA 255 99  71  255
  venetianRed       = constant $ RGBA 212 26  31  255

  -- Browns
  beige             = constant $ RGBA 163 148 128 255
  brown             = constant $ RGBA 128 42  42  255
  brownMadder       = constant $ RGBA 219 41  41  255
  brownOchre        = constant $ RGBA 135 66  31  255
  burlywood         = constant $ RGBA 222 184 135 255
  burntSienna       = constant $ RGBA 138 54  15  255
  burntUmber        = constant $ RGBA 138 51  36  255
  chocolate         = constant $ RGBA 210 105 30  255
  deepOchre         = constant $ RGBA 115 61  26  255
  flesh             = constant $ RGBA 255 125 64  255
  fleshOchre        = constant $ RGBA 255 87  33  255
  goldOchre         = constant $ RGBA 199 120 38  255
  greenishUmber     = constant $ RGBA 255 61  13  255
  khaki             = constant $ RGBA 240 230 140 255
  khakiDark         = constant $ RGBA 189 183 107 255
  lightBeige        = constant $ RGBA 245 245 220 255
  peru              = constant $ RGBA 205 133 63  255
  rosyBrown         = constant $ RGBA 188 143 143 255
  rawSienna         = constant $ RGBA 199 97  20  255
  rawUmber          = constant $ RGBA 115 74  18  255
  sepia             = constant $ RGBA 94  38  18  255
  sienna            = constant $ RGBA 160 82  45  255
  saddleBrown       = constant $ RGBA 139 69  19  255
  sandyBrown        = constant $ RGBA 244 164 96  255
  tan               = constant $ RGBA 210 180 140 255
  vanDykeBrown      = constant $ RGBA 94  38  5   255

  -- Oranges
  cadmiumOrange     = constant $ RGBA 255 97  3   255
  cadmiumRedLight   = constant $ RGBA 255 3   13  255
  carrot            = constant $ RGBA 237 145 33  255
  darkOrange        = constant $ RGBA 255 140 0   255
  marsOrange        = constant $ RGBA 150 69  20  255
  marsYellow        = constant $ RGBA 227 112 26  255
  orange            = constant $ RGBA 255 128 0   255
  orangeRed         = constant $ RGBA 255 69  0   255
  yellowOchre       = constant $ RGBA 227 130 23  255

  -- Yellows
  aureolineYellow   = constant $ RGBA 255 168 36  255
  banana            = constant $ RGBA 227 207 87  255
  cadmiumLemon      = constant $ RGBA 255 227 3   255
  cadmiumYellow     = constant $ RGBA 255 153 18  255
  gold              = constant $ RGBA 255 215 0   255
  goldenrod         = constant $ RGBA 218 165 32  255
  goldenrodDark     = constant $ RGBA 184 134 11  255
  goldenrodLight    = constant $ RGBA 250 250 210 255
  goldenrodPale     = constant $ RGBA 238 232 170 255
  lightGoldenrod    = constant $ RGBA 238 221 130 255
  melon             = constant $ RGBA 227 168 105 255
  naplesyellowdeep  = constant $ RGBA 255 168 18  255
  yellow            = constant $ RGBA 255 255 0   255
  yellowLight       = constant $ RGBA 255 255 224 255

  -- Greens
  chartreuse        = constant $ RGBA 127 255 0   255
  chromeoxidegreen  = constant $ RGBA 102 128 20  255
  cinnabarGreen     = constant $ RGBA 97  179 41  255
  cobaltGreen       = constant $ RGBA 61  145 64  255
  emeraldGreen      = constant $ RGBA 0   201 87  255
  forestGreen       = constant $ RGBA 34  139 34  255
  green             = constant $ RGBA 0   255 0   255
  greenDark         = constant $ RGBA 0   100 0   255
  greenPale         = constant $ RGBA 152 251 152 255
  greenYellow       = constant $ RGBA 173 255 47  255
  lawnGreen         = constant $ RGBA 124 252 0   255
  limeGreen         = constant $ RGBA 50  205 50  255
  mint              = constant $ RGBA 189 252 201 255
  olive             = constant $ RGBA 59  94  43  255
  oliveDrab         = constant $ RGBA 107 142 35  255
  oliveGreenDark    = constant $ RGBA 85  107 47  255
  permanentGreen    = constant $ RGBA 10  201 43  255
  sapGreen          = constant $ RGBA 48  128 20  255
  seaGreen          = constant $ RGBA 46  139 87  255
  seaGreenDark      = constant $ RGBA 143 188 143 255
  seaGreenMedium    = constant $ RGBA 60  179 113 255
  seaGreenLight     = constant $ RGBA 32  178 170 255
  springGreen       = constant $ RGBA 0   255 127 255
  springGreenmedium = constant $ RGBA 0   250 154 255
  terreVerte        = constant $ RGBA 56  94  15  255
  viridianLight     = constant $ RGBA 110 255 112 255
  yellowGreen       = constant $ RGBA 154 205 50  255

  -- Cyans
  aquamarine        = constant $ RGBA 127 255 212 255
  aquamarinemedium  = constant $ RGBA 102 205 170 255
  cyan              = constant $ RGBA 0   255 255 255
  cyanWhite         = constant $ RGBA 224 255 255 255
  turquoise         = constant $ RGBA 64  224 208 255
  turquoiseDark     = constant $ RGBA 0   206 209 255
  turquoiseMedium   = constant $ RGBA 72  209 204 255
  turquoisePale     = constant $ RGBA 175 238 238 255

  -- Blues
  aliceBlue         = constant $ RGBA 240 248 255 255
  blue              = constant $ RGBA 0   0   255 255
  blueLight         = constant $ RGBA 173 216 230 255
  blueMedium        = constant $ RGBA 0   0   205 255
  cadet             = constant $ RGBA 95  158 160 255
  cobalt            = constant $ RGBA 61  89  171 255
  cornflower        = constant $ RGBA 100 149 237 255
  cerulean          = constant $ RGBA 5   184 204 255
  dodgerBlue        = constant $ RGBA 30  144 255 255
  indigo            = constant $ RGBA 8   46  84  255
  manganeseBlue     = constant $ RGBA 3   168 158 255
  midnightBlue      = constant $ RGBA 25  25  112 255
  navy              = constant $ RGBA 0   0   128 255
  peacock           = constant $ RGBA 51  161 201 255
  powderBlue        = constant $ RGBA 176 224 230 255
  royalBlue         = constant $ RGBA 65  105 225 255
  slateBlue         = constant $ RGBA 106 90  205 255
  slateBlueDark     = constant $ RGBA 72  61  139 255
  slateBlueLight    = constant $ RGBA 132 112 255 255
  slateBlueMedium   = constant $ RGBA 123 104 238 255
  skyBlue           = constant $ RGBA 135 206 235 255
  skyBlueDeep       = constant $ RGBA 0   191 255 255
  skyBlueLight      = constant $ RGBA 135 206 250 255
  steelBlue         = constant $ RGBA 70  130 180 255
  steelBlueLight    = constant $ RGBA 176 196 222 255
  turquoiseBlue     = constant $ RGBA 0   199 140 255
  ultramarine       = constant $ RGBA 18  10  143 255

  -- Magentas
  blueViolet        = constant $ RGBA 138 43  226 255
  cobaltVioletdeep  = constant $ RGBA 145 33  158 255
  magenta           = constant $ RGBA 255 0   255 255
  orchid            = constant $ RGBA 218 112 214 255
  orchidDark        = constant $ RGBA 153 50  204 255
  orchidMedium      = constant $ RGBA 186 85  211 255
  permanentViolet   = constant $ RGBA 219 38  69  255
  plum              = constant $ RGBA 221 160 221 255
  purple            = constant $ RGBA 160 32  240 255
  purpleMedium      = constant $ RGBA 147 112 219 255
  ultramarineViolet = constant $ RGBA 92  36  110 255
  violet            = constant $ RGBA 143 94  153 255
  violetDark        = constant $ RGBA 148 0   211 255
  violetRed         = constant $ RGBA 208 32  144 255
  violetRedmedium   = constant $ RGBA 199 21  133 255
  violetRedPale     = constant $ RGBA 219 112 147 255

instance NamedColour (RGBA Float) where
  -- Whites
  antiqueWhite      = constant $ RGBA 0.9804 0.9216 0.8431 1.0000
  azure             = constant $ RGBA 0.9412 1.0000 1.0000 1.0000
  bisque            = constant $ RGBA 1.0000 0.8941 0.7686 1.0000
  blanchedAlmond    = constant $ RGBA 1.0000 0.9216 0.8039 1.0000
  cornsilk          = constant $ RGBA 1.0000 0.9725 0.8627 1.0000
  eggshell          = constant $ RGBA 0.9900 0.9000 0.7900 1.0000
  floralWhite       = constant $ RGBA 1.0000 0.9804 0.9412 1.0000
  gainsboro         = constant $ RGBA 0.8627 0.8627 0.8627 1.0000
  ghostWhite        = constant $ RGBA 0.9725 0.9725 1.0000 1.0000
  honeydew          = constant $ RGBA 0.9412 1.0000 0.9412 1.0000
  ivory             = constant $ RGBA 1.0000 1.0000 0.9412 1.0000
  lavender          = constant $ RGBA 0.9020 0.9020 0.9804 1.0000
  lavenderBlush     = constant $ RGBA 1.0000 0.9412 0.9608 1.0000
  lemonChiffon      = constant $ RGBA 1.0000 0.9804 0.8039 1.0000
  linen             = constant $ RGBA 0.9804 0.9412 0.9020 1.0000
  mintCream         = constant $ RGBA 0.9608 1.0000 0.9804 1.0000
  mistyRose         = constant $ RGBA 1.0000 0.8941 0.8824 1.0000
  moccasin          = constant $ RGBA 1.0000 0.8941 0.7098 1.0000
  navajoWhite       = constant $ RGBA 1.0000 0.8706 0.6784 1.0000
  oldLace           = constant $ RGBA 0.9922 0.9608 0.9020 1.0000
  papayaWhip        = constant $ RGBA 1.0000 0.9373 0.8353 1.0000
  peachPuff         = constant $ RGBA 1.0000 0.8549 0.7255 1.0000
  seashell          = constant $ RGBA 1.0000 0.9608 0.9333 1.0000
  snow              = constant $ RGBA 1.0000 0.9804 0.9804 1.0000
  thistle           = constant $ RGBA 0.8471 0.7490 0.8471 1.0000
  titaniumWhite     = constant $ RGBA 0.9900 1.0000 0.9400 1.0000
  wheat             = constant $ RGBA 0.9608 0.8706 0.7020 1.0000
  white             = constant $ RGBA 1.0000 1.0000 1.0000 1.0000
  whiteSmoke        = constant $ RGBA 0.9608 0.9608 0.9608 1.0000
  zincWhite         = constant $ RGBA 0.9900 0.9700 1.0000 1.0000

  -- Greys
  coldGrey          = constant $ RGBA 0.5000 0.5400 0.5300 1.0000
  dimGrey           = constant $ RGBA 0.4118 0.4118 0.4118 1.0000
  grey              = constant $ RGBA 0.7529 0.7529 0.7529 1.0000
  lightGrey         = constant $ RGBA 0.8275 0.8275 0.8275 1.0000
  slateGrey         = constant $ RGBA 0.4392 0.5020 0.5647 1.0000
  slateGreyDark     = constant $ RGBA 0.1843 0.3098 0.3098 1.0000
  slateGreyLight    = constant $ RGBA 0.4667 0.5333 0.6000 1.0000
  warmGrey          = constant $ RGBA 0.5000 0.5000 0.4100 1.0000

  -- Blacks
  black             = constant $ RGBA 0.0000 0.0000 0.0000 1.0000
  ivoryBlack        = constant $ RGBA 0.1600 0.1400 0.1300 1.0000
  lampBlack         = constant $ RGBA 0.1800 0.2800 0.2300 1.0000

  -- Reds
  alizarinCrimson   = constant $ RGBA 0.8900 0.1500 0.2100 1.0000
  brick             = constant $ RGBA 0.6100 0.4000 0.1200 1.0000
  cadmiumRedDeep    = constant $ RGBA 0.8900 0.0900 0.0500 1.0000
  coral             = constant $ RGBA 1.0000 0.4980 0.3137 1.0000
  coralLight        = constant $ RGBA 0.9412 0.5020 0.5020 1.0000
  deepPink          = constant $ RGBA 1.0000 0.0784 0.5765 1.0000
  englishRed        = constant $ RGBA 0.8300 0.2400 0.1000 1.0000
  firebrick         = constant $ RGBA 0.6980 0.1333 0.1333 1.0000
  geraniumLake      = constant $ RGBA 0.8900 0.0700 0.1900 1.0000
  hotPink           = constant $ RGBA 1.0000 0.4118 0.7059 1.0000
  indianRed         = constant $ RGBA 0.6900 0.0900 0.1200 1.0000
  lightSalmon       = constant $ RGBA 1.0000 0.6275 0.4784 1.0000
  madderLakeDeep    = constant $ RGBA 0.8900 0.1800 0.1900 1.0000
  maroon            = constant $ RGBA 0.6902 0.1882 0.3765 1.0000
  pink              = constant $ RGBA 1.0000 0.7529 0.7961 1.0000
  pinkLight         = constant $ RGBA 1.0000 0.7137 0.7569 1.0000
  raspberry         = constant $ RGBA 0.5300 0.1500 0.3400 1.0000
  red               = constant $ RGBA 1.0000 0.0000 0.0000 1.0000
  roseMadder        = constant $ RGBA 0.8900 0.2100 0.2200 1.0000
  salmon            = constant $ RGBA 0.9804 0.5020 0.4471 1.0000
  tomato            = constant $ RGBA 1.0000 0.3882 0.2784 1.0000
  venetianRed       = constant $ RGBA 0.8300 0.1000 0.1200 1.0000

  -- Browns
  beige             = constant $ RGBA 0.6400 0.5800 0.5000 1.0000
  brown             = constant $ RGBA 0.5000 0.1647 0.1647 1.0000
  brownMadder       = constant $ RGBA 0.8600 0.1600 0.1600 1.0000
  brownOchre        = constant $ RGBA 0.5300 0.2600 0.1200 1.0000
  burlywood         = constant $ RGBA 0.8706 0.7216 0.5294 1.0000
  burntSienna       = constant $ RGBA 0.5400 0.2100 0.0600 1.0000
  burntUmber        = constant $ RGBA 0.5400 0.2000 0.1400 1.0000
  chocolate         = constant $ RGBA 0.8235 0.4118 0.1176 1.0000
  deepOchre         = constant $ RGBA 0.4500 0.2400 0.1000 1.0000
  flesh             = constant $ RGBA 1.0000 0.4900 0.2500 1.0000
  fleshOchre        = constant $ RGBA 1.0000 0.3400 0.1300 1.0000
  goldOchre         = constant $ RGBA 0.7800 0.4700 0.1500 1.0000
  greenishUmber     = constant $ RGBA 1.0000 0.2400 0.0500 1.0000
  khaki             = constant $ RGBA 0.9412 0.9020 0.5490 1.0000
  khakiDark         = constant $ RGBA 0.7412 0.7176 0.4196 1.0000
  lightBeige        = constant $ RGBA 0.9608 0.9608 0.8627 1.0000
  peru              = constant $ RGBA 0.8039 0.5216 0.2471 1.0000
  rosyBrown         = constant $ RGBA 0.7373 0.5608 0.5608 1.0000
  rawSienna         = constant $ RGBA 0.7800 0.3800 0.0800 1.0000
  rawUmber          = constant $ RGBA 0.4500 0.2900 0.0700 1.0000
  sepia             = constant $ RGBA 0.3700 0.1500 0.0700 1.0000
  sienna            = constant $ RGBA 0.6275 0.3216 0.1765 1.0000
  saddleBrown       = constant $ RGBA 0.5451 0.2706 0.0745 1.0000
  sandyBrown        = constant $ RGBA 0.9569 0.6431 0.3765 1.0000
  tan               = constant $ RGBA 0.8235 0.7059 0.5490 1.0000
  vanDykeBrown      = constant $ RGBA 0.3700 0.1500 0.0200 1.0000

  -- Oranges
  cadmiumOrange     = constant $ RGBA 1.0000 0.3800 0.0100 1.0000
  cadmiumRedLight   = constant $ RGBA 1.0000 0.0100 0.0500 1.0000
  carrot            = constant $ RGBA 0.9300 0.5700 0.1300 1.0000
  darkOrange        = constant $ RGBA 1.0000 0.5490 0.0000 1.0000
  marsOrange        = constant $ RGBA 0.5900 0.2700 0.0800 1.0000
  marsYellow        = constant $ RGBA 0.8900 0.4400 0.1000 1.0000
  orange            = constant $ RGBA 1.0000 0.5000 0.0000 1.0000
  orangeRed         = constant $ RGBA 1.0000 0.2706 0.0000 1.0000
  yellowOchre       = constant $ RGBA 0.8900 0.5100 0.0900 1.0000

  -- Yellows
  aureolineYellow   = constant $ RGBA 1.0000 0.6600 0.1400 1.0000
  banana            = constant $ RGBA 0.8900 0.8100 0.3400 1.0000
  cadmiumLemon      = constant $ RGBA 1.0000 0.8900 0.0100 1.0000
  cadmiumYellow     = constant $ RGBA 1.0000 0.6000 0.0700 1.0000
  gold              = constant $ RGBA 1.0000 0.8431 0.0000 1.0000
  goldenrod         = constant $ RGBA 0.8549 0.6471 0.1255 1.0000
  goldenrodDark     = constant $ RGBA 0.7216 0.5255 0.0431 1.0000
  goldenrodLight    = constant $ RGBA 0.9804 0.9804 0.8235 1.0000
  goldenrodPale     = constant $ RGBA 0.9333 0.9098 0.6667 1.0000
  lightGoldenrod    = constant $ RGBA 0.9333 0.8667 0.5098 1.0000
  melon             = constant $ RGBA 0.8900 0.6600 0.4100 1.0000
  naplesyellowdeep  = constant $ RGBA 1.0000 0.6600 0.0700 1.0000
  yellow            = constant $ RGBA 1.0000 1.0000 0.0000 1.0000
  yellowLight       = constant $ RGBA 1.0000 1.0000 0.8784 1.0000

  -- Greens
  chartreuse        = constant $ RGBA 0.4980 1.0000 0.0000 1.0000
  chromeoxidegreen  = constant $ RGBA 0.4000 0.5000 0.0800 1.0000
  cinnabarGreen     = constant $ RGBA 0.3800 0.7000 0.1600 1.0000
  cobaltGreen       = constant $ RGBA 0.2400 0.5700 0.2500 1.0000
  emeraldGreen      = constant $ RGBA 0.0000 0.7900 0.3400 1.0000
  forestGreen       = constant $ RGBA 0.1333 0.5451 0.1333 1.0000
  green             = constant $ RGBA 0.0000 1.0000 0.0000 1.0000
  greenDark         = constant $ RGBA 0.0000 0.3922 0.0000 1.0000
  greenPale         = constant $ RGBA 0.5961 0.9843 0.5961 1.0000
  greenYellow       = constant $ RGBA 0.6784 1.0000 0.1843 1.0000
  lawnGreen         = constant $ RGBA 0.4863 0.9882 0.0000 1.0000
  limeGreen         = constant $ RGBA 0.1961 0.8039 0.1961 1.0000
  mint              = constant $ RGBA 0.7400 0.9900 0.7900 1.0000
  olive             = constant $ RGBA 0.2300 0.3700 0.1700 1.0000
  oliveDrab         = constant $ RGBA 0.4196 0.5569 0.1373 1.0000
  oliveGreenDark    = constant $ RGBA 0.3333 0.4196 0.1843 1.0000
  permanentGreen    = constant $ RGBA 0.0400 0.7900 0.1700 1.0000
  sapGreen          = constant $ RGBA 0.1900 0.5000 0.0800 1.0000
  seaGreen          = constant $ RGBA 0.1804 0.5451 0.3412 1.0000
  seaGreenDark      = constant $ RGBA 0.5608 0.7373 0.5608 1.0000
  seaGreenMedium    = constant $ RGBA 0.2353 0.7020 0.4431 1.0000
  seaGreenLight     = constant $ RGBA 0.1255 0.6980 0.6667 1.0000
  springGreen       = constant $ RGBA 0.0000 1.0000 0.4980 1.0000
  springGreenmedium = constant $ RGBA 0.0000 0.9804 0.6039 1.0000
  terreVerte        = constant $ RGBA 0.2200 0.3700 0.0600 1.0000
  viridianLight     = constant $ RGBA 0.4300 1.0000 0.4400 1.0000
  yellowGreen       = constant $ RGBA 0.6039 0.8039 0.1961 1.0000

  -- Cyans
  aquamarine        = constant $ RGBA 0.4980 1.0000 0.8314 1.0000
  aquamarinemedium  = constant $ RGBA 0.4000 0.8039 0.6667 1.0000
  cyan              = constant $ RGBA 0.0000 1.0000 1.0000 1.0000
  cyanWhite         = constant $ RGBA 0.8784 1.0000 1.0000 1.0000
  turquoise         = constant $ RGBA 0.2510 0.8784 0.8157 1.0000
  turquoiseDark     = constant $ RGBA 0.0000 0.8078 0.8196 1.0000
  turquoiseMedium   = constant $ RGBA 0.2824 0.8196 0.8000 1.0000
  turquoisePale     = constant $ RGBA 0.6863 0.9333 0.9333 1.0000

  -- Blues
  aliceBlue         = constant $ RGBA 0.9412 0.9725 1.0000 1.0000
  blue              = constant $ RGBA 0.0000 0.0000 1.0000 1.0000
  blueLight         = constant $ RGBA 0.6784 0.8471 0.9020 1.0000
  blueMedium        = constant $ RGBA 0.0000 0.0000 0.8039 1.0000
  cadet             = constant $ RGBA 0.3725 0.6196 0.6275 1.0000
  cobalt            = constant $ RGBA 0.2400 0.3500 0.6700 1.0000
  cornflower        = constant $ RGBA 0.3922 0.5843 0.9294 1.0000
  cerulean          = constant $ RGBA 0.0200 0.7200 0.8000 1.0000
  dodgerBlue        = constant $ RGBA 0.1176 0.5647 1.0000 1.0000
  indigo            = constant $ RGBA 0.0300 0.1800 0.3300 1.0000
  manganeseBlue     = constant $ RGBA 0.0100 0.6600 0.6200 1.0000
  midnightBlue      = constant $ RGBA 0.0980 0.0980 0.4392 1.0000
  navy              = constant $ RGBA 0.0000 0.0000 0.5020 1.0000
  peacock           = constant $ RGBA 0.2000 0.6300 0.7900 1.0000
  powderBlue        = constant $ RGBA 0.6902 0.8784 0.9020 1.0000
  royalBlue         = constant $ RGBA 0.2549 0.4118 0.8824 1.0000
  slateBlue         = constant $ RGBA 0.4157 0.3529 0.8039 1.0000
  slateBlueDark     = constant $ RGBA 0.2824 0.2392 0.5451 1.0000
  slateBlueLight    = constant $ RGBA 0.5176 0.4392 1.0000 1.0000
  slateBlueMedium   = constant $ RGBA 0.4824 0.4078 0.9333 1.0000
  skyBlue           = constant $ RGBA 0.5294 0.8078 0.9216 1.0000
  skyBlueDeep       = constant $ RGBA 0.0000 0.7490 1.0000 1.0000
  skyBlueLight      = constant $ RGBA 0.5294 0.8078 0.9804 1.0000
  steelBlue         = constant $ RGBA 0.2745 0.5098 0.7059 1.0000
  steelBlueLight    = constant $ RGBA 0.6902 0.7686 0.8706 1.0000
  turquoiseBlue     = constant $ RGBA 0.0000 0.7800 0.5500 1.0000
  ultramarine       = constant $ RGBA 0.0700 0.0400 0.5600 1.0000

  -- Magentas
  blueViolet        = constant $ RGBA 0.5412 0.1686 0.8863 1.0000
  cobaltVioletdeep  = constant $ RGBA 0.5700 0.1300 0.6200 1.0000
  magenta           = constant $ RGBA 1.0000 0.0000 1.0000 1.0000
  orchid            = constant $ RGBA 0.8549 0.4392 0.8392 1.0000
  orchidDark        = constant $ RGBA 0.6000 0.1961 0.8000 1.0000
  orchidMedium      = constant $ RGBA 0.7294 0.3333 0.8275 1.0000
  permanentViolet   = constant $ RGBA 0.8600 0.1500 0.2700 1.0000
  plum              = constant $ RGBA 0.8667 0.6275 0.8667 1.0000
  purple            = constant $ RGBA 0.6275 0.1255 0.9412 1.0000
  purpleMedium      = constant $ RGBA 0.5765 0.4392 0.8588 1.0000
  ultramarineViolet = constant $ RGBA 0.3600 0.1400 0.4300 1.0000
  violet            = constant $ RGBA 0.5600 0.3700 0.6000 1.0000
  violetDark        = constant $ RGBA 0.5804 0.0000 0.8275 1.0000
  violetRed         = constant $ RGBA 0.8157 0.1255 0.5647 1.0000
  violetRedmedium   = constant $ RGBA 0.7804 0.0824 0.5216 1.0000
  violetRedPale     = constant $ RGBA 0.8588 0.4392 0.5765 1.0000

