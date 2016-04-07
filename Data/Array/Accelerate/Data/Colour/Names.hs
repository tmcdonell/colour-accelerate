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

class NamedColour c where
  -- Whites
  antiqueWhite      :: c
  azure             :: c
  bisque            :: c
  blanchedAlmond    :: c
  cornsilk          :: c
  eggshell          :: c
  floralWhite       :: c
  gainsboro         :: c
  ghostWhite        :: c
  honeydew          :: c
  ivory             :: c
  lavender          :: c
  lavenderBlush     :: c
  lemonChiffon      :: c
  linen             :: c
  mintCream         :: c
  mistyRose         :: c
  moccasin          :: c
  navajoWhite       :: c
  oldLace           :: c
  papayaWhip        :: c
  peachPuff         :: c
  seashell          :: c
  snow              :: c
  thistle           :: c
  titaniumWhite     :: c
  wheat             :: c
  white             :: c
  whiteSmoke        :: c
  zincWhite         :: c

  -- Greys
  coldGrey          :: c
  dimGrey           :: c
  grey              :: c
  lightGrey         :: c
  slateGrey         :: c
  slateGreyDark     :: c
  slateGreyLight    :: c
  warmGrey          :: c

  -- Blacks
  black             :: c
  ivoryBlack        :: c
  lampBlack         :: c

  -- Reds
  alizarinCrimson   :: c
  brick             :: c
  cadmiumRedDeep    :: c
  coral             :: c
  coralLight        :: c
  deepPink          :: c
  englishRed        :: c
  firebrick         :: c
  geraniumLake      :: c
  hotPink           :: c
  indianRed         :: c
  lightSalmon       :: c
  madderLakeDeep    :: c
  maroon            :: c
  pink              :: c
  pinkLight         :: c
  raspberry         :: c
  red               :: c
  roseMadder        :: c
  salmon            :: c
  tomato            :: c
  venetianRed       :: c

  -- Browns
  beige             :: c
  brown             :: c
  brownMadder       :: c
  brownOchre        :: c
  burlywood         :: c
  burntSienna       :: c
  burntUmber        :: c
  chocolate         :: c
  deepOchre         :: c
  flesh             :: c
  fleshOchre        :: c
  goldOchre         :: c
  greenishUmber     :: c
  khaki             :: c
  khakiDark         :: c
  lightBeige        :: c
  peru              :: c
  rosyBrown         :: c
  rawSienna         :: c
  rawUmber          :: c
  sepia             :: c
  sienna            :: c
  saddleBrown       :: c
  sandyBrown        :: c
  tan               :: c
  vanDykeBrown      :: c

  -- Oranges
  cadmiumOrange     :: c
  cadmiumRedLight   :: c
  carrot            :: c
  darkOrange        :: c
  marsOrange        :: c
  marsYellow        :: c
  orange            :: c
  orangeRed         :: c
  yellowOchre       :: c

  -- Yellows
  aureolineYellow   :: c
  banana            :: c
  cadmiumLemon      :: c
  cadmiumYellow     :: c
  gold              :: c
  goldenrod         :: c
  goldenrodDark     :: c
  goldenrodLight    :: c
  goldenrodPale     :: c
  lightGoldenrod    :: c
  melon             :: c
  naplesyellowdeep  :: c
  yellow            :: c
  yellowLight       :: c

  -- Greens
  chartreuse        :: c
  chromeoxidegreen  :: c
  cinnabarGreen     :: c
  cobaltGreen       :: c
  emeraldGreen      :: c
  forestGreen       :: c
  green             :: c
  greenDark         :: c
  greenPale         :: c
  greenYellow       :: c
  lawnGreen         :: c
  limeGreen         :: c
  mint              :: c
  olive             :: c
  oliveDrab         :: c
  oliveGreenDark    :: c
  permanentGreen    :: c
  sapGreen          :: c
  seaGreen          :: c
  seaGreenDark      :: c
  seaGreenMedium    :: c
  seaGreenLight     :: c
  springGreen       :: c
  springGreenmedium :: c
  terreVerte        :: c
  viridianLight     :: c
  yellowGreen       :: c

  -- Cyans
  aquamarine        :: c
  aquamarinemedium  :: c
  cyan              :: c
  cyanWhite         :: c
  turquoise         :: c
  turquoiseDark     :: c
  turquoiseMedium   :: c
  turquoisePale     :: c

  -- Blues
  aliceBlue         :: c
  blue              :: c
  blueLight         :: c
  blueMedium        :: c
  cadet             :: c
  cobalt            :: c
  cornflower        :: c
  cerulean          :: c
  dodgerBlue        :: c
  indigo            :: c
  manganeseBlue     :: c
  midnightBlue      :: c
  navy              :: c
  peacock           :: c
  powderBlue        :: c
  royalBlue         :: c
  slateBlue         :: c
  slateBlueDark     :: c
  slateBlueLight    :: c
  slateBlueMedium   :: c
  skyBlue           :: c
  skyBlueDeep       :: c
  skyBlueLight      :: c
  steelBlue         :: c
  steelBlueLight    :: c
  turquoiseBlue     :: c
  ultramarine       :: c

  -- Magentas
  blueViolet        :: c
  cobaltVioletdeep  :: c
  magenta           :: c
  orchid            :: c
  orchidDark        :: c
  orchidMedium      :: c
  permanentViolet   :: c
  plum              :: c
  purple            :: c
  purpleMedium      :: c
  ultramarineViolet :: c
  violet            :: c
  violetDark        :: c
  violetRed         :: c
  violetRedmedium   :: c
  violetRedPale     :: c

