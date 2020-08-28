{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.Names
-- Copyright   : [2016..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Names for "familiar" colours, taken from
-- <http://paulbourke.net/texture_colour/colourspace/>
--

module Data.Array.Accelerate.Data.Colour.Names
  where

import Data.Array.Accelerate                                        ( Elt, Exp, constant )


class NamedColour c where
  -- Whites
  -- | <<samples/swatch/antiqueWhite.bmp>>
  antiqueWhite      :: c
  -- | <<samples/swatch/azure.bmp>>
  azure             :: c
  -- | <<samples/swatch/bisque.bmp>>
  bisque            :: c
  -- | <<samples/swatch/blanchedAlmond.bmp>>
  blanchedAlmond    :: c
  -- | <<samples/swatch/cornsilk.bmp>>
  cornsilk          :: c
  -- | <<samples/swatch/eggshell.bmp>>
  eggshell          :: c
  -- | <<samples/swatch/floralWhite.bmp>>
  floralWhite       :: c
  -- | <<samples/swatch/gainsboro.bmp>>
  gainsboro         :: c
  -- | <<samples/swatch/ghostWhite.bmp>>
  ghostWhite        :: c
  -- | <<samples/swatch/honeydew.bmp>>
  honeydew          :: c
  -- | <<samples/swatch/ivory.bmp>>
  ivory             :: c
  -- | <<samples/swatch/lavender.bmp>>
  lavender          :: c
  -- | <<samples/swatch/lavenderBlush.bmp>>
  lavenderBlush     :: c
  -- | <<samples/swatch/lemonChiffon.bmp>>
  lemonChiffon      :: c
  -- | <<samples/swatch/linen.bmp>>
  linen             :: c
  -- | <<samples/swatch/mintCream.bmp>>
  mintCream         :: c
  -- | <<samples/swatch/mistyRose.bmp>>
  mistyRose         :: c
  -- | <<samples/swatch/moccasin.bmp>>
  moccasin          :: c
  -- | <<samples/swatch/navajoWhite.bmp>>
  navajoWhite       :: c
  -- | <<samples/swatch/oldLace.bmp>>
  oldLace           :: c
  -- | <<samples/swatch/papayaWhip.bmp>>
  papayaWhip        :: c
  -- | <<samples/swatch/peachPuff.bmp>>
  peachPuff         :: c
  -- | <<samples/swatch/seashell.bmp>>
  seashell          :: c
  -- | <<samples/swatch/snow.bmp>>
  snow              :: c
  -- | <<samples/swatch/thistle.bmp>>
  thistle           :: c
  -- | <<samples/swatch/titaniumWhite.bmp>>
  titaniumWhite     :: c
  -- | <<samples/swatch/wheat.bmp>>
  wheat             :: c
  -- | <<samples/swatch/white.bmp>>
  white             :: c
  -- | <<samples/swatch/whiteSmoke.bmp>>
  whiteSmoke        :: c
  -- | <<samples/swatch/zincWhite.bmp>>
  zincWhite         :: c

  -- Greys
  -- | <<samples/swatch/coldGrey.bmp>>
  coldGrey          :: c
  -- | <<samples/swatch/dimGrey.bmp>>
  dimGrey           :: c
  -- | <<samples/swatch/grey.bmp>>
  grey              :: c
  -- | <<samples/swatch/lightGrey.bmp>>
  lightGrey         :: c
  -- | <<samples/swatch/slateGrey.bmp>>
  slateGrey         :: c
  -- | <<samples/swatch/slateGreyDark.bmp>>
  slateGreyDark     :: c
  -- | <<samples/swatch/slateGreyLight.bmp>>
  slateGreyLight    :: c
  -- | <<samples/swatch/warmGrey.bmp>>
  warmGrey          :: c

  -- Blacks
  -- | <<samples/swatch/black.bmp>>
  black             :: c
  -- | <<samples/swatch/ivoryBlack.bmp>>
  ivoryBlack        :: c
  -- | <<samples/swatch/lampBlack.bmp>>
  lampBlack         :: c

  -- Reds
  -- | <<samples/swatch/alizarinCrimson.bmp>>
  alizarinCrimson   :: c
  -- | <<samples/swatch/brick.bmp>>
  brick             :: c
  -- | <<samples/swatch/cadmiumRedDeep.bmp>>
  cadmiumRedDeep    :: c
  -- | <<samples/swatch/coral.bmp>>
  coral             :: c
  -- | <<samples/swatch/coralLight.bmp>>
  coralLight        :: c
  -- | <<samples/swatch/deepPink.bmp>>
  deepPink          :: c
  -- | <<samples/swatch/englishRed.bmp>>
  englishRed        :: c
  -- | <<samples/swatch/firebrick.bmp>>
  firebrick         :: c
  -- | <<samples/swatch/geraniumLake.bmp>>
  geraniumLake      :: c
  -- | <<samples/swatch/hotPink.bmp>>
  hotPink           :: c
  -- | <<samples/swatch/indianRed.bmp>>
  indianRed         :: c
  -- | <<samples/swatch/lightSalmon.bmp>>
  lightSalmon       :: c
  -- | <<samples/swatch/madderLakeDeep.bmp>>
  madderLakeDeep    :: c
  -- | <<samples/swatch/maroon.bmp>>
  maroon            :: c
  -- | <<samples/swatch/pink.bmp>>
  pink              :: c
  -- | <<samples/swatch/pinkLight.bmp>>
  pinkLight         :: c
  -- | <<samples/swatch/raspberry.bmp>>
  raspberry         :: c
  -- | <<samples/swatch/red.bmp>>
  red               :: c
  -- | <<samples/swatch/roseMadder.bmp>>
  roseMadder        :: c
  -- | <<samples/swatch/salmon.bmp>>
  salmon            :: c
  -- | <<samples/swatch/tomato.bmp>>
  tomato            :: c
  -- | <<samples/swatch/venetianRed.bmp>>
  venetianRed       :: c

  -- Browns
  -- | <<samples/swatch/beige.bmp>>
  beige             :: c
  -- | <<samples/swatch/brown.bmp>>
  brown             :: c
  -- | <<samples/swatch/brownMadder.bmp>>
  brownMadder       :: c
  -- | <<samples/swatch/brownOchre.bmp>>
  brownOchre        :: c
  -- | <<samples/swatch/burlywood.bmp>>
  burlywood         :: c
  -- | <<samples/swatch/burntSienna.bmp>>
  burntSienna       :: c
  -- | <<samples/swatch/burntUmber.bmp>>
  burntUmber        :: c
  -- | <<samples/swatch/chocolate.bmp>>
  chocolate         :: c
  -- | <<samples/swatch/deepOchre.bmp>>
  deepOchre         :: c
  -- | <<samples/swatch/flesh.bmp>>
  flesh             :: c
  -- | <<samples/swatch/fleshOchre.bmp>>
  fleshOchre        :: c
  -- | <<samples/swatch/goldOchre.bmp>>
  goldOchre         :: c
  -- | <<samples/swatch/greenishUmber.bmp>>
  greenishUmber     :: c
  -- | <<samples/swatch/khaki.bmp>>
  khaki             :: c
  -- | <<samples/swatch/khakiDark.bmp>>
  khakiDark         :: c
  -- | <<samples/swatch/lightBeige.bmp>>
  lightBeige        :: c
  -- | <<samples/swatch/peru.bmp>>
  peru              :: c
  -- | <<samples/swatch/rosyBrown.bmp>>
  rosyBrown         :: c
  -- | <<samples/swatch/rawSienna.bmp>>
  rawSienna         :: c
  -- | <<samples/swatch/rawUmber.bmp>>
  rawUmber          :: c
  -- | <<samples/swatch/sepia.bmp>>
  sepia             :: c
  -- | <<samples/swatch/sienna.bmp>>
  sienna            :: c
  -- | <<samples/swatch/saddleBrown.bmp>>
  saddleBrown       :: c
  -- | <<samples/swatch/sandyBrown.bmp>>
  sandyBrown        :: c
  -- | <<samples/swatch/tan.bmp>>
  tan               :: c
  -- | <<samples/swatch/vanDykeBrown.bmp>>
  vanDykeBrown      :: c

  -- Oranges
  -- | <<samples/swatch/cadmiumOrange.bmp>>
  cadmiumOrange     :: c
  -- | <<samples/swatch/cadmiumRedLight.bmp>>
  cadmiumRedLight   :: c
  -- | <<samples/swatch/carrot.bmp>>
  carrot            :: c
  -- | <<samples/swatch/darkOrange.bmp>>
  darkOrange        :: c
  -- | <<samples/swatch/marsOrange.bmp>>
  marsOrange        :: c
  -- | <<samples/swatch/marsYellow.bmp>>
  marsYellow        :: c
  -- | <<samples/swatch/orange.bmp>>
  orange            :: c
  -- | <<samples/swatch/orangeRed.bmp>>
  orangeRed         :: c
  -- | <<samples/swatch/yellowOchre.bmp>>
  yellowOchre       :: c

  -- Yellows
  -- | <<samples/swatch/aureolineYellow.bmp>>
  aureolineYellow   :: c
  -- | <<samples/swatch/banana.bmp>>
  banana            :: c
  -- | <<samples/swatch/cadmiumLemon.bmp>>
  cadmiumLemon      :: c
  -- | <<samples/swatch/cadmiumYellow.bmp>>
  cadmiumYellow     :: c
  -- | <<samples/swatch/gold.bmp>>
  gold              :: c
  -- | <<samples/swatch/goldenrod.bmp>>
  goldenrod         :: c
  -- | <<samples/swatch/goldenrodDark.bmp>>
  goldenrodDark     :: c
  -- | <<samples/swatch/goldenrodLight.bmp>>
  goldenrodLight    :: c
  -- | <<samples/swatch/goldenrodPale.bmp>>
  goldenrodPale     :: c
  -- | <<samples/swatch/lightGoldenrod.bmp>>
  lightGoldenrod    :: c
  -- | <<samples/swatch/melon.bmp>>
  melon             :: c
  -- | <<samples/swatch/naplesYellowDeep.bmp>>
  naplesYellowDeep  :: c
  -- | <<samples/swatch/yellow.bmp>>
  yellow            :: c
  -- | <<samples/swatch/yellowLight.bmp>>
  yellowLight       :: c

  -- Greens
  -- | <<samples/swatch/chartreuse.bmp>>
  chartreuse        :: c
  -- | <<samples/swatch/chromeoxideGreen.bmp>>
  chromeoxideGreen  :: c
  -- | <<samples/swatch/cinnabarGreen.bmp>>
  cinnabarGreen     :: c
  -- | <<samples/swatch/cobaltGreen.bmp>>
  cobaltGreen       :: c
  -- | <<samples/swatch/emeraldGreen.bmp>>
  emeraldGreen      :: c
  -- | <<samples/swatch/forestGreen.bmp>>
  forestGreen       :: c
  -- | <<samples/swatch/green.bmp>>
  green             :: c
  -- | <<samples/swatch/greenDark.bmp>>
  greenDark         :: c
  -- | <<samples/swatch/greenPale.bmp>>
  greenPale         :: c
  -- | <<samples/swatch/greenYellow.bmp>>
  greenYellow       :: c
  -- | <<samples/swatch/lawnGreen.bmp>>
  lawnGreen         :: c
  -- | <<samples/swatch/limeGreen.bmp>>
  limeGreen         :: c
  -- | <<samples/swatch/mint.bmp>>
  mint              :: c
  -- | <<samples/swatch/olive.bmp>>
  olive             :: c
  -- | <<samples/swatch/oliveDrab.bmp>>
  oliveDrab         :: c
  -- | <<samples/swatch/oliveGreenDark.bmp>>
  oliveGreenDark    :: c
  -- | <<samples/swatch/permanentGreen.bmp>>
  permanentGreen    :: c
  -- | <<samples/swatch/sapGreen.bmp>>
  sapGreen          :: c
  -- | <<samples/swatch/seaGreen.bmp>>
  seaGreen          :: c
  -- | <<samples/swatch/seaGreenDark.bmp>>
  seaGreenDark      :: c
  -- | <<samples/swatch/seaGreenMedium.bmp>>
  seaGreenMedium    :: c
  -- | <<samples/swatch/seaGreenLight.bmp>>
  seaGreenLight     :: c
  -- | <<samples/swatch/springGreen.bmp>>
  springGreen       :: c
  -- | <<samples/swatch/springGreenMedium.bmp>>
  springGreenMedium :: c
  -- | <<samples/swatch/terreVerte.bmp>>
  terreVerte        :: c
  -- | <<samples/swatch/viridianLight.bmp>>
  viridianLight     :: c
  -- | <<samples/swatch/yellowGreen.bmp>>
  yellowGreen       :: c

  -- Cyans
  -- | <<samples/swatch/aquamarine.bmp>>
  aquamarine        :: c
  -- | <<samples/swatch/aquamarineMedium.bmp>>
  aquamarineMedium  :: c
  -- | <<samples/swatch/cyan.bmp>>
  cyan              :: c
  -- | <<samples/swatch/cyanWhite.bmp>>
  cyanWhite         :: c
  -- | <<samples/swatch/turquoise.bmp>>
  turquoise         :: c
  -- | <<samples/swatch/turquoiseDark.bmp>>
  turquoiseDark     :: c
  -- | <<samples/swatch/turquoiseMedium.bmp>>
  turquoiseMedium   :: c
  -- | <<samples/swatch/turquoisePale.bmp>>
  turquoisePale     :: c

  -- Blues
  -- | <<samples/swatch/aliceBlue.bmp>>
  aliceBlue         :: c
  -- | <<samples/swatch/blue.bmp>>
  blue              :: c
  -- | <<samples/swatch/blueLight.bmp>>
  blueLight         :: c
  -- | <<samples/swatch/blueMedium.bmp>>
  blueMedium        :: c
  -- | <<samples/swatch/cadet.bmp>>
  cadet             :: c
  -- | <<samples/swatch/cobalt.bmp>>
  cobalt            :: c
  -- | <<samples/swatch/cornflower.bmp>>
  cornflower        :: c
  -- | <<samples/swatch/cerulean.bmp>>
  cerulean          :: c
  -- | <<samples/swatch/dodgerBlue.bmp>>
  dodgerBlue        :: c
  -- | <<samples/swatch/indigo.bmp>>
  indigo            :: c
  -- | <<samples/swatch/manganeseBlue.bmp>>
  manganeseBlue     :: c
  -- | <<samples/swatch/midnightBlue.bmp>>
  midnightBlue      :: c
  -- | <<samples/swatch/navy.bmp>>
  navy              :: c
  -- | <<samples/swatch/peacock.bmp>>
  peacock           :: c
  -- | <<samples/swatch/powderBlue.bmp>>
  powderBlue        :: c
  -- | <<samples/swatch/royalBlue.bmp>>
  royalBlue         :: c
  -- | <<samples/swatch/slateBlue.bmp>>
  slateBlue         :: c
  -- | <<samples/swatch/slateBlueDark.bmp>>
  slateBlueDark     :: c
  -- | <<samples/swatch/slateBlueLight.bmp>>
  slateBlueLight    :: c
  -- | <<samples/swatch/slateBlueMedium.bmp>>
  slateBlueMedium   :: c
  -- | <<samples/swatch/skyBlue.bmp>>
  skyBlue           :: c
  -- | <<samples/swatch/skyBlueDeep.bmp>>
  skyBlueDeep       :: c
  -- | <<samples/swatch/skyBlueLight.bmp>>
  skyBlueLight      :: c
  -- | <<samples/swatch/steelBlue.bmp>>
  steelBlue         :: c
  -- | <<samples/swatch/steelBlueLight.bmp>>
  steelBlueLight    :: c
  -- | <<samples/swatch/turquoiseBlue.bmp>>
  turquoiseBlue     :: c
  -- | <<samples/swatch/ultramarine.bmp>>
  ultramarine       :: c

  -- Magentas
  -- | <<samples/swatch/blueViolet.bmp>>
  blueViolet        :: c
  -- | <<samples/swatch/cobaltVioletDeep.bmp>>
  cobaltVioletDeep  :: c
  -- | <<samples/swatch/magenta.bmp>>
  magenta           :: c
  -- | <<samples/swatch/orchid.bmp>>
  orchid            :: c
  -- | <<samples/swatch/orchidDark.bmp>>
  orchidDark        :: c
  -- | <<samples/swatch/orchidMedium.bmp>>
  orchidMedium      :: c
  -- | <<samples/swatch/permanentViolet.bmp>>
  permanentViolet   :: c
  -- | <<samples/swatch/plum.bmp>>
  plum              :: c
  -- | <<samples/swatch/purple.bmp>>
  purple            :: c
  -- | <<samples/swatch/purpleMedium.bmp>>
  purpleMedium      :: c
  -- | <<samples/swatch/ultramarineViolet.bmp>>
  ultramarineViolet :: c
  -- | <<samples/swatch/violet.bmp>>
  violet            :: c
  -- | <<samples/swatch/violetDark.bmp>>
  violetDark        :: c
  -- | <<samples/swatch/violetRed.bmp>>
  violetRed         :: c
  -- | <<samples/swatch/violetRedMedium.bmp>>
  violetRedMedium   :: c
  -- | <<samples/swatch/violetRedPale.bmp>>
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
  naplesYellowDeep  = constant naplesYellowDeep
  yellow            = constant yellow
  yellowLight       = constant yellowLight

  -- Greens
  chartreuse        = constant chartreuse
  chromeoxideGreen  = constant chromeoxideGreen
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
  springGreenMedium = constant springGreenMedium
  terreVerte        = constant terreVerte
  viridianLight     = constant viridianLight
  yellowGreen       = constant yellowGreen

  -- Cyans
  aquamarine        = constant aquamarine
  aquamarineMedium  = constant aquamarineMedium
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
  cobaltVioletDeep  = constant cobaltVioletDeep
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
  violetRedMedium   = constant violetRedMedium
  violetRedPale     = constant violetRedPale

