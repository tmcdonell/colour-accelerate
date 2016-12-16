{-# LANGUAGE NoImplicitPrelude #-}
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

import Data.Array.Accelerate                              ( Elt, Exp, constant )


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


instance (Elt c, NamedColour c) => NamedColour (Exp c) where
  -- Whites
  antiqueWhite      = constant antiqueWhite
  azure             = constant azure
  bisque            = constant bisque
  blanchedAlmond    = constant blanchedAlmond
  cornsilk          = constant cornsilk
  eggshell          = constant eggshell
  floralWhite       = constant floralWhite
  gainsboro         = constant gainsboro
  ghostWhite        = constant ghostWhite
  honeydew          = constant honeydew
  ivory             = constant ivory
  lavender          = constant lavender
  lavenderBlush     = constant lavenderBlush
  lemonChiffon      = constant lemonChiffon
  linen             = constant linen
  mintCream         = constant mintCream
  mistyRose         = constant mistyRose
  moccasin          = constant moccasin
  navajoWhite       = constant navajoWhite
  oldLace           = constant oldLace
  papayaWhip        = constant papayaWhip
  peachPuff         = constant peachPuff
  seashell          = constant seashell
  snow              = constant snow
  thistle           = constant thistle
  titaniumWhite     = constant titaniumWhite
  wheat             = constant wheat
  white             = constant white
  whiteSmoke        = constant whiteSmoke
  zincWhite         = constant zincWhite

  -- Greys
  coldGrey          = constant coldGrey
  dimGrey           = constant dimGrey
  grey              = constant grey
  lightGrey         = constant lightGrey
  slateGrey         = constant slateGrey
  slateGreyDark     = constant slateGreyDark
  slateGreyLight    = constant slateGreyLight
  warmGrey          = constant warmGrey

  -- Blacks
  black             = constant black
  ivoryBlack        = constant ivoryBlack
  lampBlack         = constant lampBlack

  -- Reds
  alizarinCrimson   = constant alizarinCrimson
  brick             = constant brick
  cadmiumRedDeep    = constant cadmiumRedDeep
  coral             = constant coral
  coralLight        = constant coralLight
  deepPink          = constant deepPink
  englishRed        = constant englishRed
  firebrick         = constant firebrick
  geraniumLake      = constant geraniumLake
  hotPink           = constant hotPink
  indianRed         = constant indianRed
  lightSalmon       = constant lightSalmon
  madderLakeDeep    = constant madderLakeDeep
  maroon            = constant maroon
  pink              = constant pink
  pinkLight         = constant pinkLight
  raspberry         = constant raspberry
  red               = constant red
  roseMadder        = constant roseMadder
  salmon            = constant salmon
  tomato            = constant tomato
  venetianRed       = constant venetianRed

  -- Browns
  beige             = constant beige
  brown             = constant brown
  brownMadder       = constant brownMadder
  brownOchre        = constant brownOchre
  burlywood         = constant burlywood
  burntSienna       = constant burntSienna
  burntUmber        = constant burntUmber
  chocolate         = constant chocolate
  deepOchre         = constant deepOchre
  flesh             = constant flesh
  fleshOchre        = constant fleshOchre
  goldOchre         = constant goldOchre
  greenishUmber     = constant greenishUmber
  khaki             = constant khaki
  khakiDark         = constant khakiDark
  lightBeige        = constant lightBeige
  peru              = constant peru
  rosyBrown         = constant rosyBrown
  rawSienna         = constant rawSienna
  rawUmber          = constant rawUmber
  sepia             = constant sepia
  sienna            = constant sienna
  saddleBrown       = constant saddleBrown
  sandyBrown        = constant sandyBrown
  tan               = constant tan
  vanDykeBrown      = constant vanDykeBrown

  -- Oranges
  cadmiumOrange     = constant cadmiumOrange
  cadmiumRedLight   = constant cadmiumRedLight
  carrot            = constant carrot
  darkOrange        = constant darkOrange
  marsOrange        = constant marsOrange
  marsYellow        = constant marsYellow
  orange            = constant orange
  orangeRed         = constant orangeRed
  yellowOchre       = constant yellowOchre

  -- Yellows
  aureolineYellow   = constant aureolineYellow
  banana            = constant banana
  cadmiumLemon      = constant cadmiumLemon
  cadmiumYellow     = constant cadmiumYellow
  gold              = constant gold
  goldenrod         = constant goldenrod
  goldenrodDark     = constant goldenrodDark
  goldenrodLight    = constant goldenrodLight
  goldenrodPale     = constant goldenrodPale
  lightGoldenrod    = constant lightGoldenrod
  melon             = constant melon
  naplesyellowdeep  = constant naplesyellowdeep
  yellow            = constant yellow
  yellowLight       = constant yellowLight

  -- Greens
  chartreuse        = constant chartreuse
  chromeoxidegreen  = constant chromeoxidegreen
  cinnabarGreen     = constant cinnabarGreen
  cobaltGreen       = constant cobaltGreen
  emeraldGreen      = constant emeraldGreen
  forestGreen       = constant forestGreen
  green             = constant green
  greenDark         = constant greenDark
  greenPale         = constant greenPale
  greenYellow       = constant greenYellow
  lawnGreen         = constant lawnGreen
  limeGreen         = constant limeGreen
  mint              = constant mint
  olive             = constant olive
  oliveDrab         = constant oliveDrab
  oliveGreenDark    = constant oliveGreenDark
  permanentGreen    = constant permanentGreen
  sapGreen          = constant sapGreen
  seaGreen          = constant seaGreen
  seaGreenDark      = constant seaGreenDark
  seaGreenMedium    = constant seaGreenMedium
  seaGreenLight     = constant seaGreenLight
  springGreen       = constant springGreen
  springGreenmedium = constant springGreenmedium
  terreVerte        = constant terreVerte
  viridianLight     = constant viridianLight
  yellowGreen       = constant yellowGreen

  -- Cyans
  aquamarine        = constant aquamarine
  aquamarinemedium  = constant aquamarinemedium
  cyan              = constant cyan
  cyanWhite         = constant cyanWhite
  turquoise         = constant turquoise
  turquoiseDark     = constant turquoiseDark
  turquoiseMedium   = constant turquoiseMedium
  turquoisePale     = constant turquoisePale

  -- Blues
  aliceBlue         = constant aliceBlue
  blue              = constant blue
  blueLight         = constant blueLight
  blueMedium        = constant blueMedium
  cadet             = constant cadet
  cobalt            = constant cobalt
  cornflower        = constant cornflower
  cerulean          = constant cerulean
  dodgerBlue        = constant dodgerBlue
  indigo            = constant indigo
  manganeseBlue     = constant manganeseBlue
  midnightBlue      = constant midnightBlue
  navy              = constant navy
  peacock           = constant peacock
  powderBlue        = constant powderBlue
  royalBlue         = constant royalBlue
  slateBlue         = constant slateBlue
  slateBlueDark     = constant slateBlueDark
  slateBlueLight    = constant slateBlueLight
  slateBlueMedium   = constant slateBlueMedium
  skyBlue           = constant skyBlue
  skyBlueDeep       = constant skyBlueDeep
  skyBlueLight      = constant skyBlueLight
  steelBlue         = constant steelBlue
  steelBlueLight    = constant steelBlueLight
  turquoiseBlue     = constant turquoiseBlue
  ultramarine       = constant ultramarine

  -- Magentas
  blueViolet        = constant blueViolet
  cobaltVioletdeep  = constant cobaltVioletdeep
  magenta           = constant magenta
  orchid            = constant orchid
  orchidDark        = constant orchidDark
  orchidMedium      = constant orchidMedium
  permanentViolet   = constant permanentViolet
  plum              = constant plum
  purple            = constant purple
  purpleMedium      = constant purpleMedium
  ultramarineViolet = constant ultramarineViolet
  violet            = constant violet
  violetDark        = constant violetDark
  violetRed         = constant violetRed
  violetRedmedium   = constant violetRedmedium
  violetRedPale     = constant violetRedPale

