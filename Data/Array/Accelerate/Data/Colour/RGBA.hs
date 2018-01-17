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
-- Module      : Data.Array.Accelerate.Data.Colour.RGBA
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
  clamp,
  blend,
  luminance,
  opacity, opaque, transparent,

  packRGBA,  packABGR,  unpackRGBA,  unpackABGR,
  packRGBA8, packABGR8, unpackRGBA8, unpackABGR8,

) where

import Data.Array.Accelerate                                        as A hiding ( clamp )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product                                ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar                            ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.Names
import Data.Array.Accelerate.Data.Colour.Internal.Pack

import Data.Typeable
import Prelude                                                      as P


-- | An RGBA colour value.
--
type Colour = RGBA Float


-- | Construct an RGBA colour from individual channel components. The components
-- will be clamped to the range [0..1].
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


-- | Luminance of an RGB colour (Y component of a YUV colour).
--
luminance :: Exp Colour -> Exp Float
luminance (unlift -> RGBA r g b _) = 0.299*r + 0.587*g + 0.114*b


-- | Set the opacity of the given colour. The opacity is clamped to the range
-- [0..1].
--
opacity :: Exp Float -> Exp Colour -> Exp Colour
opacity a (unlift -> RGBA r g b _) = lift $ RGBA r g b (0 `A.max` a `A.min` 1)

-- | Make colour transparent
--
transparent :: Exp Colour -> Exp Colour
transparent = opacity 0

-- | A completely opaque colour
--
opaque :: Exp Colour -> Exp Colour
opaque = opacity 1


-- Packed representation
-- ---------------------

-- | Convert a Colour into a packed-word RGBA representation
--
packRGBA :: Exp Colour -> Exp Word32
packRGBA (unlift -> RGBA r g b a) =
  pack8 (word8OfFloat r) (word8OfFloat g) (word8OfFloat b) (word8OfFloat a)

-- | Convert a colour into a packed-word ABGR representation
--
packABGR :: Exp Colour -> Exp Word32
packABGR (unlift -> RGBA r g b a) =
  pack8 (word8OfFloat a) (word8OfFloat b) (word8OfFloat g) (word8OfFloat r)

packRGBA8 :: Exp (RGBA Word8) -> Exp Word32
packRGBA8 (unlift -> RGBA r g b a) = pack8 r g b a

packABGR8 :: Exp (RGBA Word8) -> Exp Word32
packABGR8 (unlift -> RGBA r g b a) = pack8 a b g r


-- | Convert a colour from a packed-word RGBA representation
--
unpackRGBA :: Exp Word32 -> Exp Colour
unpackRGBA w =
  let (r,g,b,a::Exp Word8) = unlift (unpack8 w)
  in  rgba8 r g b a

-- | Convert a colour from a packed-word ABGR representation
--
unpackABGR :: Exp Word32 -> Exp Colour
unpackABGR w =
  let (a,b,g,r::Exp Word8) = unlift (unpack8 w)
  in  rgba8 r g b a

unpackRGBA8 :: Exp Word32 -> Exp (RGBA Word8)
unpackRGBA8 w =
  let (r,g,b,a::Exp Word8) = unlift (unpack8 w)
  in  lift $ RGBA r g b a

unpackABGR8 :: Exp Word32 -> Exp (RGBA Word8)
unpackABGR8 w =
  let (a,b,g,r::Exp Word8) = unlift (unpack8 w)
  in  lift $ RGBA r g b a


-- Accelerate bits
-- ---------------

-- | An RGBA colour value to hold the colour components. All components lie in
-- the range [0..1).
--
-- We need to parameterise by a type so that we can have both Exp (RGBA a) and
-- RGBA (Exp a).
--
data RGBA a = RGBA a a a a
  deriving (Show, P.Eq, Functor, Typeable)

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

instance P.Num a => P.Num (RGBA a) where
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
        = let f = P.fromInteger i
          in  RGBA f f f 1

instance (P.Num a, P.Fractional a) => P.Fractional (RGBA a) where
  (/) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1/r2) (g1/g2) (b1/b2) 1

  recip (RGBA r1 g1 b1 _)
        = RGBA (recip r1) (recip g1) (recip b1) 1

  fromRational r
        = let f = P.fromRational r
          in  RGBA f f f 1

instance {-# OVERLAPS #-} A.Num a => P.Num (Exp (RGBA a)) where
  (+)           = lift2 ((+) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (-)           = lift2 ((-) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (*)           = lift2 ((*) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  abs           = lift1 (abs :: RGBA (Exp a) -> RGBA (Exp a))
  signum        = lift1 (signum :: RGBA (Exp a) -> RGBA (Exp a))
  fromInteger i = let f = P.fromInteger i
                      a = P.fromInteger 1 :: Exp a
                  in lift $ RGBA f f f a

instance {-# OVERLAPS #-} A.Fractional a => P.Fractional (Exp (RGBA a)) where
  (/)            = lift2 ((/) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  recip          = lift1 (recip :: RGBA (Exp a) -> RGBA (Exp a))
  fromRational r = let f = P.fromRational r
                       a = P.fromRational 1 :: Exp a
                   in lift $ RGBA f f f a


-- Named colours
-- -------------

instance NamedColour (RGBA Word8) where
  -- Whites
  antiqueWhite      = RGBA 250 235 215 255
  azure             = RGBA 240 255 255 255
  bisque            = RGBA 255 228 196 255
  blanchedAlmond    = RGBA 255 235 205 255
  cornsilk          = RGBA 255 248 220 255
  eggshell          = RGBA 252 230 201 255
  floralWhite       = RGBA 255 250 240 255
  gainsboro         = RGBA 220 220 220 255
  ghostWhite        = RGBA 248 248 255 255
  honeydew          = RGBA 240 255 240 255
  ivory             = RGBA 255 255 240 255
  lavender          = RGBA 230 230 250 255
  lavenderBlush     = RGBA 255 240 245 255
  lemonChiffon      = RGBA 255 250 205 255
  linen             = RGBA 250 240 230 255
  mintCream         = RGBA 245 255 250 255
  mistyRose         = RGBA 255 228 225 255
  moccasin          = RGBA 255 228 181 255
  navajoWhite       = RGBA 255 222 173 255
  oldLace           = RGBA 253 245 230 255
  papayaWhip        = RGBA 255 239 213 255
  peachPuff         = RGBA 255 218 185 255
  seashell          = RGBA 255 245 238 255
  snow              = RGBA 255 250 250 255
  thistle           = RGBA 216 191 216 255
  titaniumWhite     = RGBA 252 255 240 255
  wheat             = RGBA 245 222 179 255
  white             = RGBA 255 255 255 255
  whiteSmoke        = RGBA 245 245 245 255
  zincWhite         = RGBA 253 248 255 255

  -- Greys
  coldGrey          = RGBA 128 138 135 255
  dimGrey           = RGBA 105 105 105 255
  grey              = RGBA 192 192 192 255
  lightGrey         = RGBA 211 211 211 255
  slateGrey         = RGBA 112 128 144 255
  slateGreyDark     = RGBA 47  79  79  255
  slateGreyLight    = RGBA 119 136 153 255
  warmGrey          = RGBA 128 128 105 255

  -- Blacks
  black             = RGBA 0   0   0   255
  ivoryBlack        = RGBA 41  36  33  255
  lampBlack         = RGBA 46  71  59  255

  -- Reds
  alizarinCrimson   = RGBA 227 38  54  255
  brick             = RGBA 156 102 31  255
  cadmiumRedDeep    = RGBA 227 23  13  255
  coral             = RGBA 255 127 80  255
  coralLight        = RGBA 240 128 128 255
  deepPink          = RGBA 255 20  147 255
  englishRed        = RGBA 212 61  26  255
  firebrick         = RGBA 178 34  34  255
  geraniumLake      = RGBA 227 18  48  255
  hotPink           = RGBA 255 105 180 255
  indianRed         = RGBA 176 23  31  255
  lightSalmon       = RGBA 255 160 122 255
  madderLakeDeep    = RGBA 227 46  48  255
  maroon            = RGBA 176 48  96  255
  pink              = RGBA 255 192 203 255
  pinkLight         = RGBA 255 182 193 255
  raspberry         = RGBA 135 38  87  255
  red               = RGBA 255 0   0   255
  roseMadder        = RGBA 227 54  56  255
  salmon            = RGBA 250 128 114 255
  tomato            = RGBA 255 99  71  255
  venetianRed       = RGBA 212 26  31  255

  -- Browns
  beige             = RGBA 163 148 128 255
  brown             = RGBA 128 42  42  255
  brownMadder       = RGBA 219 41  41  255
  brownOchre        = RGBA 135 66  31  255
  burlywood         = RGBA 222 184 135 255
  burntSienna       = RGBA 138 54  15  255
  burntUmber        = RGBA 138 51  36  255
  chocolate         = RGBA 210 105 30  255
  deepOchre         = RGBA 115 61  26  255
  flesh             = RGBA 255 125 64  255
  fleshOchre        = RGBA 255 87  33  255
  goldOchre         = RGBA 199 120 38  255
  greenishUmber     = RGBA 255 61  13  255
  khaki             = RGBA 240 230 140 255
  khakiDark         = RGBA 189 183 107 255
  lightBeige        = RGBA 245 245 220 255
  peru              = RGBA 205 133 63  255
  rosyBrown         = RGBA 188 143 143 255
  rawSienna         = RGBA 199 97  20  255
  rawUmber          = RGBA 115 74  18  255
  sepia             = RGBA 94  38  18  255
  sienna            = RGBA 160 82  45  255
  saddleBrown       = RGBA 139 69  19  255
  sandyBrown        = RGBA 244 164 96  255
  tan               = RGBA 210 180 140 255
  vanDykeBrown      = RGBA 94  38  5   255

  -- Oranges
  cadmiumOrange     = RGBA 255 97  3   255
  cadmiumRedLight   = RGBA 255 3   13  255
  carrot            = RGBA 237 145 33  255
  darkOrange        = RGBA 255 140 0   255
  marsOrange        = RGBA 150 69  20  255
  marsYellow        = RGBA 227 112 26  255
  orange            = RGBA 255 128 0   255
  orangeRed         = RGBA 255 69  0   255
  yellowOchre       = RGBA 227 130 23  255

  -- Yellows
  aureolineYellow   = RGBA 255 168 36  255
  banana            = RGBA 227 207 87  255
  cadmiumLemon      = RGBA 255 227 3   255
  cadmiumYellow     = RGBA 255 153 18  255
  gold              = RGBA 255 215 0   255
  goldenrod         = RGBA 218 165 32  255
  goldenrodDark     = RGBA 184 134 11  255
  goldenrodLight    = RGBA 250 250 210 255
  goldenrodPale     = RGBA 238 232 170 255
  lightGoldenrod    = RGBA 238 221 130 255
  melon             = RGBA 227 168 105 255
  naplesYellowDeep  = RGBA 255 168 18  255
  yellow            = RGBA 255 255 0   255
  yellowLight       = RGBA 255 255 224 255

  -- Greens
  chartreuse        = RGBA 127 255 0   255
  chromeoxideGreen  = RGBA 102 128 20  255
  cinnabarGreen     = RGBA 97  179 41  255
  cobaltGreen       = RGBA 61  145 64  255
  emeraldGreen      = RGBA 0   201 87  255
  forestGreen       = RGBA 34  139 34  255
  green             = RGBA 0   255 0   255
  greenDark         = RGBA 0   100 0   255
  greenPale         = RGBA 152 251 152 255
  greenYellow       = RGBA 173 255 47  255
  lawnGreen         = RGBA 124 252 0   255
  limeGreen         = RGBA 50  205 50  255
  mint              = RGBA 189 252 201 255
  olive             = RGBA 59  94  43  255
  oliveDrab         = RGBA 107 142 35  255
  oliveGreenDark    = RGBA 85  107 47  255
  permanentGreen    = RGBA 10  201 43  255
  sapGreen          = RGBA 48  128 20  255
  seaGreen          = RGBA 46  139 87  255
  seaGreenDark      = RGBA 143 188 143 255
  seaGreenMedium    = RGBA 60  179 113 255
  seaGreenLight     = RGBA 32  178 170 255
  springGreen       = RGBA 0   255 127 255
  springGreenMedium = RGBA 0   250 154 255
  terreVerte        = RGBA 56  94  15  255
  viridianLight     = RGBA 110 255 112 255
  yellowGreen       = RGBA 154 205 50  255

  -- Cyans
  aquamarine        = RGBA 127 255 212 255
  aquamarineMedium  = RGBA 102 205 170 255
  cyan              = RGBA 0   255 255 255
  cyanWhite         = RGBA 224 255 255 255
  turquoise         = RGBA 64  224 208 255
  turquoiseDark     = RGBA 0   206 209 255
  turquoiseMedium   = RGBA 72  209 204 255
  turquoisePale     = RGBA 175 238 238 255

  -- Blues
  aliceBlue         = RGBA 240 248 255 255
  blue              = RGBA 0   0   255 255
  blueLight         = RGBA 173 216 230 255
  blueMedium        = RGBA 0   0   205 255
  cadet             = RGBA 95  158 160 255
  cobalt            = RGBA 61  89  171 255
  cornflower        = RGBA 100 149 237 255
  cerulean          = RGBA 5   184 204 255
  dodgerBlue        = RGBA 30  144 255 255
  indigo            = RGBA 8   46  84  255
  manganeseBlue     = RGBA 3   168 158 255
  midnightBlue      = RGBA 25  25  112 255
  navy              = RGBA 0   0   128 255
  peacock           = RGBA 51  161 201 255
  powderBlue        = RGBA 176 224 230 255
  royalBlue         = RGBA 65  105 225 255
  slateBlue         = RGBA 106 90  205 255
  slateBlueDark     = RGBA 72  61  139 255
  slateBlueLight    = RGBA 132 112 255 255
  slateBlueMedium   = RGBA 123 104 238 255
  skyBlue           = RGBA 135 206 235 255
  skyBlueDeep       = RGBA 0   191 255 255
  skyBlueLight      = RGBA 135 206 250 255
  steelBlue         = RGBA 70  130 180 255
  steelBlueLight    = RGBA 176 196 222 255
  turquoiseBlue     = RGBA 0   199 140 255
  ultramarine       = RGBA 18  10  143 255

  -- Magentas
  blueViolet        = RGBA 138 43  226 255
  cobaltVioletDeep  = RGBA 145 33  158 255
  magenta           = RGBA 255 0   255 255
  orchid            = RGBA 218 112 214 255
  orchidDark        = RGBA 153 50  204 255
  orchidMedium      = RGBA 186 85  211 255
  permanentViolet   = RGBA 219 38  69  255
  plum              = RGBA 221 160 221 255
  purple            = RGBA 160 32  240 255
  purpleMedium      = RGBA 147 112 219 255
  ultramarineViolet = RGBA 92  36  110 255
  violet            = RGBA 143 94  153 255
  violetDark        = RGBA 148 0   211 255
  violetRed         = RGBA 208 32  144 255
  violetRedMedium   = RGBA 199 21  133 255
  violetRedPale     = RGBA 219 112 147 255

instance NamedColour (RGBA Float) where
  -- Whites
  antiqueWhite      = RGBA 0.9804 0.9216 0.8431 1.0000
  azure             = RGBA 0.9412 1.0000 1.0000 1.0000
  bisque            = RGBA 1.0000 0.8941 0.7686 1.0000
  blanchedAlmond    = RGBA 1.0000 0.9216 0.8039 1.0000
  cornsilk          = RGBA 1.0000 0.9725 0.8627 1.0000
  eggshell          = RGBA 0.9900 0.9000 0.7900 1.0000
  floralWhite       = RGBA 1.0000 0.9804 0.9412 1.0000
  gainsboro         = RGBA 0.8627 0.8627 0.8627 1.0000
  ghostWhite        = RGBA 0.9725 0.9725 1.0000 1.0000
  honeydew          = RGBA 0.9412 1.0000 0.9412 1.0000
  ivory             = RGBA 1.0000 1.0000 0.9412 1.0000
  lavender          = RGBA 0.9020 0.9020 0.9804 1.0000
  lavenderBlush     = RGBA 1.0000 0.9412 0.9608 1.0000
  lemonChiffon      = RGBA 1.0000 0.9804 0.8039 1.0000
  linen             = RGBA 0.9804 0.9412 0.9020 1.0000
  mintCream         = RGBA 0.9608 1.0000 0.9804 1.0000
  mistyRose         = RGBA 1.0000 0.8941 0.8824 1.0000
  moccasin          = RGBA 1.0000 0.8941 0.7098 1.0000
  navajoWhite       = RGBA 1.0000 0.8706 0.6784 1.0000
  oldLace           = RGBA 0.9922 0.9608 0.9020 1.0000
  papayaWhip        = RGBA 1.0000 0.9373 0.8353 1.0000
  peachPuff         = RGBA 1.0000 0.8549 0.7255 1.0000
  seashell          = RGBA 1.0000 0.9608 0.9333 1.0000
  snow              = RGBA 1.0000 0.9804 0.9804 1.0000
  thistle           = RGBA 0.8471 0.7490 0.8471 1.0000
  titaniumWhite     = RGBA 0.9900 1.0000 0.9400 1.0000
  wheat             = RGBA 0.9608 0.8706 0.7020 1.0000
  white             = RGBA 1.0000 1.0000 1.0000 1.0000
  whiteSmoke        = RGBA 0.9608 0.9608 0.9608 1.0000
  zincWhite         = RGBA 0.9900 0.9700 1.0000 1.0000

  -- Greys
  coldGrey          = RGBA 0.5000 0.5400 0.5300 1.0000
  dimGrey           = RGBA 0.4118 0.4118 0.4118 1.0000
  grey              = RGBA 0.7529 0.7529 0.7529 1.0000
  lightGrey         = RGBA 0.8275 0.8275 0.8275 1.0000
  slateGrey         = RGBA 0.4392 0.5020 0.5647 1.0000
  slateGreyDark     = RGBA 0.1843 0.3098 0.3098 1.0000
  slateGreyLight    = RGBA 0.4667 0.5333 0.6000 1.0000
  warmGrey          = RGBA 0.5000 0.5000 0.4100 1.0000

  -- Blacks
  black             = RGBA 0.0000 0.0000 0.0000 1.0000
  ivoryBlack        = RGBA 0.1600 0.1400 0.1300 1.0000
  lampBlack         = RGBA 0.1800 0.2800 0.2300 1.0000

  -- Reds
  alizarinCrimson   = RGBA 0.8900 0.1500 0.2100 1.0000
  brick             = RGBA 0.6100 0.4000 0.1200 1.0000
  cadmiumRedDeep    = RGBA 0.8900 0.0900 0.0500 1.0000
  coral             = RGBA 1.0000 0.4980 0.3137 1.0000
  coralLight        = RGBA 0.9412 0.5020 0.5020 1.0000
  deepPink          = RGBA 1.0000 0.0784 0.5765 1.0000
  englishRed        = RGBA 0.8300 0.2400 0.1000 1.0000
  firebrick         = RGBA 0.6980 0.1333 0.1333 1.0000
  geraniumLake      = RGBA 0.8900 0.0700 0.1900 1.0000
  hotPink           = RGBA 1.0000 0.4118 0.7059 1.0000
  indianRed         = RGBA 0.6900 0.0900 0.1200 1.0000
  lightSalmon       = RGBA 1.0000 0.6275 0.4784 1.0000
  madderLakeDeep    = RGBA 0.8900 0.1800 0.1900 1.0000
  maroon            = RGBA 0.6902 0.1882 0.3765 1.0000
  pink              = RGBA 1.0000 0.7529 0.7961 1.0000
  pinkLight         = RGBA 1.0000 0.7137 0.7569 1.0000
  raspberry         = RGBA 0.5300 0.1500 0.3400 1.0000
  red               = RGBA 1.0000 0.0000 0.0000 1.0000
  roseMadder        = RGBA 0.8900 0.2100 0.2200 1.0000
  salmon            = RGBA 0.9804 0.5020 0.4471 1.0000
  tomato            = RGBA 1.0000 0.3882 0.2784 1.0000
  venetianRed       = RGBA 0.8300 0.1000 0.1200 1.0000

  -- Browns
  beige             = RGBA 0.6400 0.5800 0.5000 1.0000
  brown             = RGBA 0.5000 0.1647 0.1647 1.0000
  brownMadder       = RGBA 0.8600 0.1600 0.1600 1.0000
  brownOchre        = RGBA 0.5300 0.2600 0.1200 1.0000
  burlywood         = RGBA 0.8706 0.7216 0.5294 1.0000
  burntSienna       = RGBA 0.5400 0.2100 0.0600 1.0000
  burntUmber        = RGBA 0.5400 0.2000 0.1400 1.0000
  chocolate         = RGBA 0.8235 0.4118 0.1176 1.0000
  deepOchre         = RGBA 0.4500 0.2400 0.1000 1.0000
  flesh             = RGBA 1.0000 0.4900 0.2500 1.0000
  fleshOchre        = RGBA 1.0000 0.3400 0.1300 1.0000
  goldOchre         = RGBA 0.7800 0.4700 0.1500 1.0000
  greenishUmber     = RGBA 1.0000 0.2400 0.0500 1.0000
  khaki             = RGBA 0.9412 0.9020 0.5490 1.0000
  khakiDark         = RGBA 0.7412 0.7176 0.4196 1.0000
  lightBeige        = RGBA 0.9608 0.9608 0.8627 1.0000
  peru              = RGBA 0.8039 0.5216 0.2471 1.0000
  rosyBrown         = RGBA 0.7373 0.5608 0.5608 1.0000
  rawSienna         = RGBA 0.7800 0.3800 0.0800 1.0000
  rawUmber          = RGBA 0.4500 0.2900 0.0700 1.0000
  sepia             = RGBA 0.3700 0.1500 0.0700 1.0000
  sienna            = RGBA 0.6275 0.3216 0.1765 1.0000
  saddleBrown       = RGBA 0.5451 0.2706 0.0745 1.0000
  sandyBrown        = RGBA 0.9569 0.6431 0.3765 1.0000
  tan               = RGBA 0.8235 0.7059 0.5490 1.0000
  vanDykeBrown      = RGBA 0.3700 0.1500 0.0200 1.0000

  -- Oranges
  cadmiumOrange     = RGBA 1.0000 0.3800 0.0100 1.0000
  cadmiumRedLight   = RGBA 1.0000 0.0100 0.0500 1.0000
  carrot            = RGBA 0.9300 0.5700 0.1300 1.0000
  darkOrange        = RGBA 1.0000 0.5490 0.0000 1.0000
  marsOrange        = RGBA 0.5900 0.2700 0.0800 1.0000
  marsYellow        = RGBA 0.8900 0.4400 0.1000 1.0000
  orange            = RGBA 1.0000 0.5000 0.0000 1.0000
  orangeRed         = RGBA 1.0000 0.2706 0.0000 1.0000
  yellowOchre       = RGBA 0.8900 0.5100 0.0900 1.0000

  -- Yellows
  aureolineYellow   = RGBA 1.0000 0.6600 0.1400 1.0000
  banana            = RGBA 0.8900 0.8100 0.3400 1.0000
  cadmiumLemon      = RGBA 1.0000 0.8900 0.0100 1.0000
  cadmiumYellow     = RGBA 1.0000 0.6000 0.0700 1.0000
  gold              = RGBA 1.0000 0.8431 0.0000 1.0000
  goldenrod         = RGBA 0.8549 0.6471 0.1255 1.0000
  goldenrodDark     = RGBA 0.7216 0.5255 0.0431 1.0000
  goldenrodLight    = RGBA 0.9804 0.9804 0.8235 1.0000
  goldenrodPale     = RGBA 0.9333 0.9098 0.6667 1.0000
  lightGoldenrod    = RGBA 0.9333 0.8667 0.5098 1.0000
  melon             = RGBA 0.8900 0.6600 0.4100 1.0000
  naplesYellowDeep  = RGBA 1.0000 0.6600 0.0700 1.0000
  yellow            = RGBA 1.0000 1.0000 0.0000 1.0000
  yellowLight       = RGBA 1.0000 1.0000 0.8784 1.0000

  -- Greens
  chartreuse        = RGBA 0.4980 1.0000 0.0000 1.0000
  chromeoxideGreen  = RGBA 0.4000 0.5000 0.0800 1.0000
  cinnabarGreen     = RGBA 0.3800 0.7000 0.1600 1.0000
  cobaltGreen       = RGBA 0.2400 0.5700 0.2500 1.0000
  emeraldGreen      = RGBA 0.0000 0.7900 0.3400 1.0000
  forestGreen       = RGBA 0.1333 0.5451 0.1333 1.0000
  green             = RGBA 0.0000 1.0000 0.0000 1.0000
  greenDark         = RGBA 0.0000 0.3922 0.0000 1.0000
  greenPale         = RGBA 0.5961 0.9843 0.5961 1.0000
  greenYellow       = RGBA 0.6784 1.0000 0.1843 1.0000
  lawnGreen         = RGBA 0.4863 0.9882 0.0000 1.0000
  limeGreen         = RGBA 0.1961 0.8039 0.1961 1.0000
  mint              = RGBA 0.7400 0.9900 0.7900 1.0000
  olive             = RGBA 0.2300 0.3700 0.1700 1.0000
  oliveDrab         = RGBA 0.4196 0.5569 0.1373 1.0000
  oliveGreenDark    = RGBA 0.3333 0.4196 0.1843 1.0000
  permanentGreen    = RGBA 0.0400 0.7900 0.1700 1.0000
  sapGreen          = RGBA 0.1900 0.5000 0.0800 1.0000
  seaGreen          = RGBA 0.1804 0.5451 0.3412 1.0000
  seaGreenDark      = RGBA 0.5608 0.7373 0.5608 1.0000
  seaGreenMedium    = RGBA 0.2353 0.7020 0.4431 1.0000
  seaGreenLight     = RGBA 0.1255 0.6980 0.6667 1.0000
  springGreen       = RGBA 0.0000 1.0000 0.4980 1.0000
  springGreenMedium = RGBA 0.0000 0.9804 0.6039 1.0000
  terreVerte        = RGBA 0.2200 0.3700 0.0600 1.0000
  viridianLight     = RGBA 0.4300 1.0000 0.4400 1.0000
  yellowGreen       = RGBA 0.6039 0.8039 0.1961 1.0000

  -- Cyans
  aquamarine        = RGBA 0.4980 1.0000 0.8314 1.0000
  aquamarineMedium  = RGBA 0.4000 0.8039 0.6667 1.0000
  cyan              = RGBA 0.0000 1.0000 1.0000 1.0000
  cyanWhite         = RGBA 0.8784 1.0000 1.0000 1.0000
  turquoise         = RGBA 0.2510 0.8784 0.8157 1.0000
  turquoiseDark     = RGBA 0.0000 0.8078 0.8196 1.0000
  turquoiseMedium   = RGBA 0.2824 0.8196 0.8000 1.0000
  turquoisePale     = RGBA 0.6863 0.9333 0.9333 1.0000

  -- Blues
  aliceBlue         = RGBA 0.9412 0.9725 1.0000 1.0000
  blue              = RGBA 0.0000 0.0000 1.0000 1.0000
  blueLight         = RGBA 0.6784 0.8471 0.9020 1.0000
  blueMedium        = RGBA 0.0000 0.0000 0.8039 1.0000
  cadet             = RGBA 0.3725 0.6196 0.6275 1.0000
  cobalt            = RGBA 0.2400 0.3500 0.6700 1.0000
  cornflower        = RGBA 0.3922 0.5843 0.9294 1.0000
  cerulean          = RGBA 0.0200 0.7200 0.8000 1.0000
  dodgerBlue        = RGBA 0.1176 0.5647 1.0000 1.0000
  indigo            = RGBA 0.0300 0.1800 0.3300 1.0000
  manganeseBlue     = RGBA 0.0100 0.6600 0.6200 1.0000
  midnightBlue      = RGBA 0.0980 0.0980 0.4392 1.0000
  navy              = RGBA 0.0000 0.0000 0.5020 1.0000
  peacock           = RGBA 0.2000 0.6300 0.7900 1.0000
  powderBlue        = RGBA 0.6902 0.8784 0.9020 1.0000
  royalBlue         = RGBA 0.2549 0.4118 0.8824 1.0000
  slateBlue         = RGBA 0.4157 0.3529 0.8039 1.0000
  slateBlueDark     = RGBA 0.2824 0.2392 0.5451 1.0000
  slateBlueLight    = RGBA 0.5176 0.4392 1.0000 1.0000
  slateBlueMedium   = RGBA 0.4824 0.4078 0.9333 1.0000
  skyBlue           = RGBA 0.5294 0.8078 0.9216 1.0000
  skyBlueDeep       = RGBA 0.0000 0.7490 1.0000 1.0000
  skyBlueLight      = RGBA 0.5294 0.8078 0.9804 1.0000
  steelBlue         = RGBA 0.2745 0.5098 0.7059 1.0000
  steelBlueLight    = RGBA 0.6902 0.7686 0.8706 1.0000
  turquoiseBlue     = RGBA 0.0000 0.7800 0.5500 1.0000
  ultramarine       = RGBA 0.0700 0.0400 0.5600 1.0000

  -- Magentas
  blueViolet        = RGBA 0.5412 0.1686 0.8863 1.0000
  cobaltVioletDeep  = RGBA 0.5700 0.1300 0.6200 1.0000
  magenta           = RGBA 1.0000 0.0000 1.0000 1.0000
  orchid            = RGBA 0.8549 0.4392 0.8392 1.0000
  orchidDark        = RGBA 0.6000 0.1961 0.8000 1.0000
  orchidMedium      = RGBA 0.7294 0.3333 0.8275 1.0000
  permanentViolet   = RGBA 0.8600 0.1500 0.2700 1.0000
  plum              = RGBA 0.8667 0.6275 0.8667 1.0000
  purple            = RGBA 0.6275 0.1255 0.9412 1.0000
  purpleMedium      = RGBA 0.5765 0.4392 0.8588 1.0000
  ultramarineViolet = RGBA 0.3600 0.1400 0.4300 1.0000
  violet            = RGBA 0.5600 0.3700 0.6000 1.0000
  violetDark        = RGBA 0.5804 0.0000 0.8275 1.0000
  violetRed         = RGBA 0.8157 0.1255 0.5647 1.0000
  violetRedMedium   = RGBA 0.7804 0.0824 0.5216 1.0000
  violetRedPale     = RGBA 0.8588 0.4392 0.5765 1.0000

