-- |
-- Module      : Data.Array.Accelerate.Data.Colour.Names
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Names for "familiar" colours, taken from
-- <http://paulbourke.net/texture_colour/colourspace/>
--

module Data.Array.Accelerate.Data.Colour.Names
  where

import Data.Array.Accelerate

class NamedColour c where
  -- Whites
  antiqueWhite      :: Exp c
  azure             :: Exp c
  bisque            :: Exp c
  blanchedAlmond    :: Exp c
  cornsilk          :: Exp c
  eggshell          :: Exp c
  floralWhite       :: Exp c
  gainsboro         :: Exp c
  ghostWhite        :: Exp c
  honeydew          :: Exp c
  ivory             :: Exp c
  lavender          :: Exp c
  lavenderBlush     :: Exp c
  lemonChiffon      :: Exp c
  linen             :: Exp c
  mintCream         :: Exp c
  mistyRose         :: Exp c
  moccasin          :: Exp c
  navajoWhite       :: Exp c
  oldLace           :: Exp c
  papayaWhip        :: Exp c
  peachPuff         :: Exp c
  seashell          :: Exp c
  snow              :: Exp c
  thistle           :: Exp c
  titaniumWhite     :: Exp c
  wheat             :: Exp c
  white             :: Exp c
  whiteSmoke        :: Exp c
  zincWhite         :: Exp c

  -- Greys
  coldGrey          :: Exp c
  dimGrey           :: Exp c
  grey              :: Exp c
  lightGrey         :: Exp c
  slateGrey         :: Exp c
  slateGreyDark     :: Exp c
  slateGreyLight    :: Exp c
  warmGrey          :: Exp c

  -- Blacks
  black             :: Exp c
  ivoryBlack        :: Exp c
  lampBlack         :: Exp c

  -- Reds
  alizarinCrimson   :: Exp c
  brick             :: Exp c
  cadmiumRedDeep    :: Exp c
  coral             :: Exp c
  coralLight        :: Exp c
  deepPink          :: Exp c
  englishRed        :: Exp c
  firebrick         :: Exp c
  geraniumLake      :: Exp c
  hotPink           :: Exp c
  indianRed         :: Exp c
  lightSalmon       :: Exp c
  madderLakeDeep    :: Exp c
  maroon            :: Exp c
  pink              :: Exp c
  pinkLight         :: Exp c
  raspberry         :: Exp c
  red               :: Exp c
  roseMadder        :: Exp c
  salmon            :: Exp c
  tomato            :: Exp c
  venetianRed       :: Exp c

  -- Browns
  beige             :: Exp c
  brown             :: Exp c
  brownMadder       :: Exp c
  brownOchre        :: Exp c
  burlywood         :: Exp c
  burntSienna       :: Exp c
  burntUmber        :: Exp c
  chocolate         :: Exp c
  deepOchre         :: Exp c
  flesh             :: Exp c
  fleshOchre        :: Exp c
  goldOchre         :: Exp c
  greenishUmber     :: Exp c
  khaki             :: Exp c
  khakiDark         :: Exp c
  lightBeige        :: Exp c
  peru              :: Exp c
  rosyBrown         :: Exp c
  rawSienna         :: Exp c
  rawUmber          :: Exp c
  sepia             :: Exp c
  sienna            :: Exp c
  saddleBrown       :: Exp c
  sandyBrown        :: Exp c
  tan               :: Exp c
  vanDykeBrown      :: Exp c

  -- Oranges
  cadmiumOrange     :: Exp c
  cadmiumRedLight   :: Exp c
  carrot            :: Exp c
  darkOrange        :: Exp c
  marsOrange        :: Exp c
  marsYellow        :: Exp c
  orange            :: Exp c
  orangeRed         :: Exp c
  yellowOchre       :: Exp c

  -- Yellows
  aureolineYellow   :: Exp c
  banana            :: Exp c
  cadmiumLemon      :: Exp c
  cadmiumYellow     :: Exp c
  gold              :: Exp c
  goldenrod         :: Exp c
  goldenrodDark     :: Exp c
  goldenrodLight    :: Exp c
  goldenrodPale     :: Exp c
  lightGoldenrod    :: Exp c
  melon             :: Exp c
  naplesyellowdeep  :: Exp c
  yellow            :: Exp c
  yellowLight       :: Exp c

  -- Greens
  chartreuse        :: Exp c
  chromeoxidegreen  :: Exp c
  cinnabarGreen     :: Exp c
  cobaltGreen       :: Exp c
  emeraldGreen      :: Exp c
  forestGreen       :: Exp c
  green             :: Exp c
  greenDark         :: Exp c
  greenPale         :: Exp c
  greenYellow       :: Exp c
  lawnGreen         :: Exp c
  limeGreen         :: Exp c
  mint              :: Exp c
  olive             :: Exp c
  oliveDrab         :: Exp c
  oliveGreenDark    :: Exp c
  permanentGreen    :: Exp c
  sapGreen          :: Exp c
  seaGreen          :: Exp c
  seaGreenDark      :: Exp c
  seaGreenMedium    :: Exp c
  seaGreenLight     :: Exp c
  springGreen       :: Exp c
  springGreenmedium :: Exp c
  terreVerte        :: Exp c
  viridianLight     :: Exp c
  yellowGreen       :: Exp c

  -- Cyans
  aquamarine        :: Exp c
  aquamarinemedium  :: Exp c
  cyan              :: Exp c
  cyanWhite         :: Exp c
  turquoise         :: Exp c
  turquoiseDark     :: Exp c
  turquoiseMedium   :: Exp c
  turquoisePale     :: Exp c

  -- Blues
  aliceBlue         :: Exp c
  blue              :: Exp c
  blueLight         :: Exp c
  blueMedium        :: Exp c
  cadet             :: Exp c
  cobalt            :: Exp c
  cornflower        :: Exp c
  cerulean          :: Exp c
  dodgerBlue        :: Exp c
  indigo            :: Exp c
  manganeseBlue     :: Exp c
  midnightBlue      :: Exp c
  navy              :: Exp c
  peacock           :: Exp c
  powderBlue        :: Exp c
  royalBlue         :: Exp c
  slateBlue         :: Exp c
  slateBlueDark     :: Exp c
  slateBlueLight    :: Exp c
  slateBlueMedium   :: Exp c
  skyBlue           :: Exp c
  skyBlueDeep       :: Exp c
  skyBlueLight      :: Exp c
  steelBlue         :: Exp c
  steelBlueLight    :: Exp c
  turquoiseBlue     :: Exp c
  ultramarine       :: Exp c

  -- Magentas
  blueViolet        :: Exp c
  cobaltVioletdeep  :: Exp c
  magenta           :: Exp c
  orchid            :: Exp c
  orchidDark        :: Exp c
  orchidMedium      :: Exp c
  permanentViolet   :: Exp c
  plum              :: Exp c
  purple            :: Exp c
  purpleMedium      :: Exp c
  ultramarineViolet :: Exp c
  violet            :: Exp c
  violetDark        :: Exp c
  violetRed         :: Exp c
  violetRedmedium   :: Exp c
  violetRedPale     :: Exp c

