{-# LANGUAGE ViewPatterns #-}
--
-- Example for generating a HSV colour map
--
--            0                             hue                             360
--           +----------------------------------------------------------------->
--         0 |                             black                               |
--           |                                                                 |
--           |                                                                 |
-- sat=1 val |                                                                 |
--           |                                                                 |
--           | red              green                  blue                red |
--         1 v                                                                 |
--         0 |         yellow              cyan                 magenta        |
--           |                                                                 |
--           |                                                                 |
-- val=1 sat |                                                                 |
--           |                                                                 |
--           |                            white                                |
--         1 v-----------------------------------------------------------------+

module Main where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.IO                           as A  -- package: accelerate-io
import Data.Array.Accelerate.Interpreter                  as A

import Data.Array.Accelerate.Data.Colour.HSV              as HSV
import Data.Array.Accelerate.Data.Colour.RGB              ( packBGR )

import Prelude                                            as P


zoom, width, height :: Int
zoom   = 4
width  = 360 * zoom
height = 100 * 2 * zoom


picker :: Acc (Array DIM2 Colour)
picker = A.generate (constant (Z :. height :. width)) palette
  where
    palette :: Exp DIM2 -> Exp Colour
    palette (unlift -> Z :. y :. x) =
      let
          c  = A.fromIntegral y / P.fromIntegral height
          h  = A.fromIntegral x / P.fromIntegral width * 360
          sv = c A.>* 0.5 ? ( lift (constant 1, (1-c)*2)
                            , lift (c*2, constant 1) )
      in
      lift $ HSV h (A.fst sv) (A.snd sv)


main :: IO ()
main
  = writeImageToBMP "hsv.bmp"
  $ run $ A.map (packBGR . toRGB) picker

