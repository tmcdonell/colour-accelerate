{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
--
-- Example for generating a hot-to-cold colour ramp
-- <http://paulbourke.net/texture_colour/colourspace/>
--
--
--               (max+2*min)/4                         (3*max+min)/4
-- min                              (max+min)/2                              max
-- +------------------+------------------+------------------+------------------+
-- |                                                                           |
-- |                                                                           |
-- +------------------+------------------+------------------+------------------+
-- blue             cyan               green              yellow             red
--

module Main where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.IO                           as A  -- package: accelerate-io
import Data.Array.Accelerate.Interpreter                  as A

import Data.Array.Accelerate.Data.Colour.RGB              as RGB

import Prelude                                            as P

zoom, width, height :: Int
zoom   = 10
width  = 100 * zoom
height = 10  * zoom


-- The "standard" hot-to-cold colour map. Given a scalar v in the range
-- [vmin..vmax], each colour component ranges from 0 (no contribution) to
-- 1 (fully saturated).
--
ramp :: Exp Float     -- minimum value
     -> Exp Float     -- maximum value
     -> Exp Float     -- point in the range
     -> Exp Colour
ramp vmin vmax (min vmax . max vmin -> v)
  = v A.<* vmin + 0.25 * dv ? ( lift (RGB 0 (4 * (v - vmin) / dv) 1)
  , v A.<* vmin + 0.50 * dv ? ( lift (RGB 0 1 (1 + 4 * (vmin + 0.25 * dv - v) / dv ))
  , v A.<* vmin + 0.75 * dv ? ( lift (RGB (4 * (v - vmin - 0.5 * dv) / dv) 1 0)
  , {- otherwise -}             lift (RGB 1 (1 + 4 * (vmin + 0.75 * dv - v) / dv) 0) )))
  where
    dv = vmax - vmin

picker :: Acc (Array DIM2 Colour)
picker = A.generate (constant (Z :. height :. width)) palette
  where
    palette :: Exp DIM2 -> Exp Colour
    palette (unlift -> Z :. (_::Exp Int) :. x) =
      ramp 0 (P.fromIntegral width) (A.fromIntegral x)

main :: IO ()
main
  = writeImageToBMP "hot.bmp"
  $ run $ A.map packBGR picker

