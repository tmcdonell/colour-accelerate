{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.HSV
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Colours in the HSV (hue-saturation-value) cylindrical-coordinate
-- representation of points in the RGB colour space.
--

module Data.Array.Accelerate.Data.Colour.HSV (

  Colour,
  HSV(..),

  hsv,
  toRGB, fromRGB,
  hue,
  saturation,
  value,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Array.Accelerate.Data.Colour.RGB    ( RGB(..) )
import Data.Array.Accelerate.Data.Colour.Names  as C

import Data.Typeable
import Prelude                                  as P


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
    f = A.fromIntegral (A.floor (n / d) :: Exp Int)

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
    rgb = h' A.<* 1 ? ( lift (RGB c' x' m)
        , h' A.<* 2 ? ( lift (RGB x' c' m)
        , h' A.<* 3 ? ( lift (RGB m  c' x')
        , h' A.<* 4 ? ( lift (RGB m  x' c')
        , h' A.<* 5 ? ( lift (RGB x' m  c')
        ,             ( lift (RGB c' m  x') ))))))


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
    s  = mx A.>*  0 ? ( c / mx , 0 )
    h  = c  A./=* 0 ? ( h0 * 60, 0 )
    --
    h0 = mx ==* r ? ( ((g-b)/c) `fmod` 6
       , mx ==* g ? ( ((b-r)/c) + 2
       , mx ==* b ? ( ((r-g)/c) + 4
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
  deriving (Show, Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 3-tuple
--
type instance EltRepr (HSV a) = EltRepr (a, a, a)

instance Elt a => Elt (HSV a) where
  eltType (_ :: HSV a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (h,s,v) = toElt c in HSV h s v
  fromElt (HSV h s v)           = fromElt (h,s,v)

instance Elt a => IsProduct Elt (HSV a) where
  type ProdRepr (HSV a)          = ((((),a), a), a)
  fromProd _ (HSV h s v)         = ((((), h), s), v)
  toProd _ ((((),h),s),v)        = HSV h s v
  prod cst _                     = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (HSV a) where
  type Plain (HSV a)    = HSV (Plain a)
  lift (HSV h s v)      = Exp . Tuple $ NilTup `SnocTup` lift h `SnocTup` lift s `SnocTup` lift v

instance Elt a => Unlift Exp (HSV (Exp a)) where
  unlift c      = let h = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      s = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      v = Exp $ ZeroTupIdx `Prj` c
                  in HSV h s v

{--
instance Num a => Num (HSV a) where
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
        = let f = fromInteger i
          in  HSV f f f


instance (Elt a, IsNum a) => Num (Exp (HSV a)) where
  (+)           = lift2 ((+) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  (-)           = lift2 ((-) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  (*)           = lift2 ((*) :: HSV (Exp a) -> HSV (Exp a) -> HSV (Exp a))
  abs           = lift1 (abs :: HSV (Exp a) -> HSV (Exp a))
  signum        = lift1 (signum :: HSV (Exp a) -> HSV (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                  in lift $ HSV f f f
--}


-- Named colours
-- -------------

{--
instance NamedColour (HSV Float) where
  -- Whites
  antiqueWhite      = fromRGB antiqueWhite
  azure             = fromRGB azure
  bisque            = fromRGB bisque
  blanchedAlmond    = fromRGB blanchedAlmond
  cornsilk          = fromRGB cornsilk
  eggshell          = fromRGB eggshell
  floralWhite       = fromRGB floralWhite
  gainsboro         = fromRGB gainsboro
  ghostWhite        = fromRGB ghostWhite
  honeydew          = fromRGB honeydew
  ivory             = fromRGB ivory
  lavender          = fromRGB lavender
  lavenderBlush     = fromRGB lavenderBlush
  lemonChiffon      = fromRGB lemonChiffon
  linen             = fromRGB linen
  mintCream         = fromRGB mintCream
  mistyRose         = fromRGB mistyRose
  moccasin          = fromRGB moccasin
  navajoWhite       = fromRGB navajoWhite
  oldLace           = fromRGB oldLace
  papayaWhip        = fromRGB papayaWhip
  peachPuff         = fromRGB peachPuff
  seashell          = fromRGB seashell
  snow              = fromRGB snow
  thistle           = fromRGB thistle
  titaniumWhite     = fromRGB titaniumWhite
  wheat             = fromRGB wheat
  white             = fromRGB white
  whiteSmoke        = fromRGB whiteSmoke
  zincWhite         = fromRGB zincWhite

  -- Greys
  coldGrey          = fromRGB coldGrey
  dimGrey           = fromRGB dimGrey
  grey              = fromRGB grey
  lightGrey         = fromRGB lightGrey
  slateGrey         = fromRGB slateGrey
  slateGreyDark     = fromRGB slateGreyDark
  slateGreyLight    = fromRGB slateGreyLight
  warmGrey          = fromRGB warmGrey

  -- Blacks
  black             = fromRGB black
  ivoryBlack        = fromRGB ivoryBlack
  lampBlack         = fromRGB lampBlack

  -- Reds
  alizarinCrimson   = fromRGB alizarinCrimson
  brick             = fromRGB brick
  cadmiumRedDeep    = fromRGB cadmiumRedDeep
  coral             = fromRGB coral
  coralLight        = fromRGB coralLight
  deepPink          = fromRGB deepPink
  englishRed        = fromRGB englishRed
  firebrick         = fromRGB firebrick
  geraniumLake      = fromRGB geraniumLake
  hotPink           = fromRGB hotPink
  indianRed         = fromRGB indianRed
  lightSalmon       = fromRGB lightSalmon
  madderLakeDeep    = fromRGB madderLakeDeep
  maroon            = fromRGB maroon
  pink              = fromRGB pink
  pinkLight         = fromRGB pinkLight
  raspberry         = fromRGB raspberry
  red               = fromRGB red
  roseMadder        = fromRGB roseMadder
  salmon            = fromRGB salmon
  tomato            = fromRGB tomato
  venetianRed       = fromRGB venetianRed

  -- Browns
  beige             = fromRGB beige
  brown             = fromRGB brown
  brownMadder       = fromRGB brownMadder
  brownOchre        = fromRGB brownOchre
  burlywood         = fromRGB burlywood
  burntSienna       = fromRGB burntSienna
  burntUmber        = fromRGB burntUmber
  chocolate         = fromRGB chocolate
  deepOchre         = fromRGB deepOchre
  flesh             = fromRGB flesh
  fleshOchre        = fromRGB fleshOchre
  goldOchre         = fromRGB goldOchre
  greenishUmber     = fromRGB greenishUmber
  khaki             = fromRGB khaki
  khakiDark         = fromRGB khakiDark
  lightBeige        = fromRGB lightBeige
  peru              = fromRGB peru
  rosyBrown         = fromRGB rosyBrown
  rawSienna         = fromRGB rawSienna
  rawUmber          = fromRGB rawUmber
  sepia             = fromRGB sepia
  sienna            = fromRGB sienna
  saddleBrown       = fromRGB saddleBrown
  sandyBrown        = fromRGB sandyBrown
  tan               = fromRGB C.tan
  vanDykeBrown      = fromRGB vanDykeBrown

  -- Oranges
  cadmiumOrange     = fromRGB cadmiumOrange
  cadmiumRedLight   = fromRGB cadmiumRedLight
  carrot            = fromRGB carrot
  darkOrange        = fromRGB darkOrange
  marsOrange        = fromRGB marsOrange
  marsYellow        = fromRGB marsYellow
  orange            = fromRGB orange
  orangeRed         = fromRGB orangeRed
  yellowOchre       = fromRGB yellowOchre

  -- Yellows
  aureolineYellow   = fromRGB aureolineYellow
  banana            = fromRGB banana
  cadmiumLemon      = fromRGB cadmiumLemon
  cadmiumYellow     = fromRGB cadmiumYellow
  gold              = fromRGB gold
  goldenrod         = fromRGB goldenrod
  goldenrodDark     = fromRGB goldenrodDark
  goldenrodLight    = fromRGB goldenrodLight
  goldenrodPale     = fromRGB goldenrodPale
  lightGoldenrod    = fromRGB lightGoldenrod
  melon             = fromRGB melon
  naplesyellowdeep  = fromRGB naplesyellowdeep
  yellow            = fromRGB yellow
  yellowLight       = fromRGB yellowLight

  -- Greens
  chartreuse        = fromRGB chartreuse
  chromeoxidegreen  = fromRGB chromeoxidegreen
  cinnabarGreen     = fromRGB cinnabarGreen
  cobaltGreen       = fromRGB cobaltGreen
  emeraldGreen      = fromRGB emeraldGreen
  forestGreen       = fromRGB forestGreen
  green             = fromRGB green
  greenDark         = fromRGB greenDark
  greenPale         = fromRGB greenPale
  greenYellow       = fromRGB greenYellow
  lawnGreen         = fromRGB lawnGreen
  limeGreen         = fromRGB limeGreen
  mint              = fromRGB mint
  olive             = fromRGB olive
  oliveDrab         = fromRGB oliveDrab
  oliveGreenDark    = fromRGB oliveGreenDark
  permanentGreen    = fromRGB permanentGreen
  sapGreen          = fromRGB sapGreen
  seaGreen          = fromRGB seaGreen
  seaGreenDark      = fromRGB seaGreenDark
  seaGreenMedium    = fromRGB seaGreenMedium
  seaGreenLight     = fromRGB seaGreenLight
  springGreen       = fromRGB springGreen
  springGreenmedium = fromRGB springGreenmedium
  terreVerte        = fromRGB terreVerte
  viridianLight     = fromRGB viridianLight
  yellowGreen       = fromRGB yellowGreen

  -- Cyans
  aquamarine        = fromRGB aquamarine
  aquamarinemedium  = fromRGB aquamarinemedium
  cyan              = fromRGB cyan
  cyanWhite         = fromRGB cyanWhite
  turquoise         = fromRGB turquoise
  turquoiseDark     = fromRGB turquoiseDark
  turquoiseMedium   = fromRGB turquoiseMedium
  turquoisePale     = fromRGB turquoisePale

  -- Blues
  aliceBlue         = fromRGB aliceBlue
  blue              = fromRGB blue
  blueLight         = fromRGB blueLight
  blueMedium        = fromRGB blueMedium
  cadet             = fromRGB cadet
  cobalt            = fromRGB cobalt
  cornflower        = fromRGB cornflower
  cerulean          = fromRGB cerulean
  dodgerBlue        = fromRGB dodgerBlue
  indigo            = fromRGB indigo
  manganeseBlue     = fromRGB manganeseBlue
  midnightBlue      = fromRGB midnightBlue
  navy              = fromRGB navy
  peacock           = fromRGB peacock
  powderBlue        = fromRGB powderBlue
  royalBlue         = fromRGB royalBlue
  slateBlue         = fromRGB slateBlue
  slateBlueDark     = fromRGB slateBlueDark
  slateBlueLight    = fromRGB slateBlueLight
  slateBlueMedium   = fromRGB slateBlueMedium
  skyBlue           = fromRGB skyBlue
  skyBlueDeep       = fromRGB skyBlueDeep
  skyBlueLight      = fromRGB skyBlueLight
  steelBlue         = fromRGB steelBlue
  steelBlueLight    = fromRGB steelBlueLight
  turquoiseBlue     = fromRGB turquoiseBlue
  ultramarine       = fromRGB ultramarine

  -- Magentas
  blueViolet        = fromRGB blueViolet
  cobaltVioletdeep  = fromRGB cobaltVioletdeep
  magenta           = fromRGB magenta
  orchid            = fromRGB orchid
  orchidDark        = fromRGB orchidDark
  orchidMedium      = fromRGB orchidMedium
  permanentViolet   = fromRGB permanentViolet
  plum              = fromRGB plum
  purple            = fromRGB purple
  purpleMedium      = fromRGB purpleMedium
  ultramarineViolet = fromRGB ultramarineViolet
  violet            = fromRGB violet
  violetDark        = fromRGB violetDark
  violetRed         = fromRGB violetRed
  violetRedmedium   = fromRGB violetRedmedium
  violetRedPale     = fromRGB violetRedPale
--}

