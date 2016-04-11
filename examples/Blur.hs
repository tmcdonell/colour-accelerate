{-# LANGUAGE FlexibleContexts #-}
--
-- Example demonstrating artefacts caused by combining colour values in the
-- non-linear RGB colour space.
--
-- Test image (and more information) available at:
--
-- <http://ninedegreesbelow.com/photography/linear-gamma-blur-normal-blend.html>
--
-- Note in the given test image (blocks100100.bmp) the boundaries between the
-- different colours:
--
--  * In the linear-gamma sRGB colour space colours blend smoothly.
--
--  * In the non-linear gamma RGB image, there are dark regions separating red
--    from green and cyan, and blue from red and green; purple lines separate
--    cyan from red and magenta; green separates yellow from cyan. These dark
--    lines are artefacts produces from mixing colours in the non-linear RGB
--    colour space.
--

module Main where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Interpreter                  as A
import Data.Array.Accelerate.IO                           as A        -- package: accelerate-io
import Data.Array.Accelerate.Data.Colour.RGB              as RGB
import Data.Array.Accelerate.Data.Colour.SRGB             as SRGB

import Control.Monad
import System.FilePath
import System.Directory
import System.Environment
import Prelude                                            as P


type Image a            = Array DIM2 a

type Stencil5x1 a       = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a       = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)


convolve5x1 :: (Elt a, Num (Exp a)) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (Elt a, Num (Exp a)) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]


-- Separable 5x5 Gaussian blur in the x- and y-directions
--
gaussianX :: Acc (Image (RGB Float)) -> Acc (Image (RGB Float))
gaussianX = stencil (convolve5x1 gaussian) Clamp
  where
    gaussian = P.map (/16) [ 1, 4, 6, 4, 1 ]

gaussianY :: Acc (Image (RGB Float)) -> Acc (Image (RGB Float))
gaussianY = stencil (convolve1x5 gaussian) Clamp
  where
    gaussian = P.map (/16) [ 1, 4, 6, 4, 1 ]



main :: IO ()
main = do
  argv        <- getArgs
  let inputFile = case argv of
                    []  -> "blocks100100.bmp"
                    f:_ -> f

  exists      <- doesFileExist inputFile
  unless exists $
    error $ unlines [ "usage: blur <file.bmp>"
                    , ""
                    , "If no input image is specified, the default 'blocks100100.bmp'"
                    , "in the current directory will be used (if available)."
                    ]

  input       <- either (error . show) id `fmap` readImageFromBMP inputFile

  let name f    = "blur_" P.++ f <.> "bmp"

      img       = A.map unpackRGB (use input)
      blur      = gaussianY . gaussianX

      rgb_blur  = blur img
      srgb_blur = A.map toRGB . blur . A.map fromRGB $ img

  writeImageToBMP (name "rgb")  $ run $ A.map packRGB rgb_blur
  writeImageToBMP (name "srgb") $ run $ A.map packRGB srgb_blur

