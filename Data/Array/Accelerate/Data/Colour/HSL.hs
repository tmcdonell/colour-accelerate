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
    c x = 0 `max` x `min` 1

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
  deriving (Show, Eq, Functor, Typeable)

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

instance Num a => Num (HSL a) where
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


instance (Elt a, IsNum a) => Num (Exp (HSL a)) where
  (+)           = lift2 ((+) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  (-)           = lift2 ((-) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  (*)           = lift2 ((*) :: HSL (Exp a) -> HSL (Exp a) -> HSL (Exp a))
  abs           = lift1 (abs :: HSL (Exp a) -> HSL (Exp a))
  signum        = lift1 (signum :: HSL (Exp a) -> HSL (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                  in lift $ HSL f f f


-- Named colours
-- -------------

{--
instance NamedColour (HSL Float) where
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


