{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
--
-- <http://stackoverflow.com/questions/16500656/which-color-gradient-is-used-to-color-mandelbrot-in-wikipedia>
-- <https://en.wikipedia.org/wiki/Monotone_cubic_interpolation>
-- <https://en.wikipedia.org/wiki/Cubic_Hermite_spline>
--

module Main where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.IO                           as A  -- package: accelerate-io
-- import Data.Array.Accelerate.Interpreter                  as A
import Data.Array.Accelerate.LLVM.Native                  as A

import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.Names

import qualified Prelude                                  as P

width, height :: Int
width  = 1000
height = 100


ultra :: Exp Float -> Exp Colour
ultra p =
  if p <= p1 then interp (p0,p1) (c0,c1) (m0,m1) p else
  if p <= p2 then interp (p1,p2) (c1,c2) (m1,m2) p else
  if p <= p3 then interp (p2,p3) (c2,c3) (m2,m3) p else
  if p <= p4 then interp (p3,p4) (c3,c4) (m3,m4) p else
                  interp (p4,p5) (c4,c5) (m4,m5) p
  where
    p0 = 0.0     ; c0 = rgb8 0   7   100  ; m0 = (0.7843138, 2.4509804, 2.52451)
    p1 = 0.16    ; c1 = rgb8 32  107 203  ; m1 = (1.93816,   2.341629,  1.6544118)
    p2 = 0.42    ; c2 = rgb8 237 255 255  ; m2 = (1.7046283, 0.0,        0.0)
    p3 = 0.6425  ; c3 = rgb8 255 170 0    ; m3 = (0.0,       -2.2812111, 0.0)
    p4 = 0.8575  ; c4 = rgb8 0   2   0    ; m4 = (0.0,       0.0,        0.0)
    p5 = 1.0     ; c5 = c0                ; m5 = m0

    -- interpolate between each of the RGB components
    interp :: (Exp Float, Exp Float)
           -> (Exp Colour, Exp Colour)
           -> ((Exp Float,Exp Float,Exp Float),(Exp Float,Exp Float,Exp Float))
           -> Exp Float
           -> Exp Colour
    interp (x0,x1) (unlift -> RGB r0 g0 b0, unlift -> RGB r1 g1 b1) ((mr0,mg0,mb0),(mr1,mg1,mb1)) x =
      rgb (linear (x0,x1) (r0,r1) (mr0,mr1) x)
          (linear (x0,x1) (g0,g1) (mg0,mg1) x)
          (linear (x0,x1) (b0,b1) (mb0,mb1) x)

-- cubic interpolation
cubic :: (Exp Float, Exp Float)
      -> (Exp Float, Exp Float)
      -> (Exp Float, Exp Float)
      -> Exp Float
      -> Exp Float
cubic (x0,x1) (y0,y1) (m0,m1) x =
  let
      -- basis functions for cubic hermite spine
      h_00 = (1 + 2*t) * (1 - t) ** 2
      h_10 = t * (1 - t) ** 2
      h_01 = t ** 2 * (3 - 2 * t)
      h_11 = t ** 2 * (t - 1)
      --
      h    = x1 - x0
      t    = (x - x0) / h
  in
  y0 * h_00 + h * m0 * h_10 + y1 * h_01 + h * m1 * h_11

-- linear interpolation
linear :: (Exp Float, Exp Float)
       -> (Exp Float, Exp Float)
       -> (Exp Float, Exp Float)
       -> Exp Float
       -> Exp Float
linear (x0,x1) (y0,y1) _ x =
  y0 + (x - x0) * (y1 - y0) / (x1 - x0)


picker :: Acc (Array DIM2 Colour)
picker = A.generate (constant (Z :. height :. width)) palette
  where
    palette :: Exp DIM2 -> Exp Colour
    palette (unlift -> Z :. y :. x) =
      let
          c         = ultra (A.fromIntegral x / P.fromIntegral width)
          h         = toFloating y / P.fromIntegral height
          RGB r g b = unlift c
          det       = 1 / (255 * 0.75)
      in
      if abs (r-h) < det then red   else
      if abs (g-h) < det then green else
      if abs (b-h) < det then blue  else
                              c

main :: P.IO ()
main
  = writeImageToBMP "ultra.bmp"
  $ run $ A.map packRGB picker


{--
-- Monotone cubic interpolation
-- ----------------------------
--
-- <https://en.wikipedia.org/wiki/Monotone_cubic_interpolation>
--

ps :: [Float]
ps = [ 0.0 , 0.16 , 0.42 , 0.6425 , 0.8575, 1.0 ]

rs, gs, bs :: [Float]
rs = P.map (\x -> P.fromIntegral x / 255) [   0,  32, 237, 255,   0,   0 ]
gs = P.map (\x -> P.fromIntegral x / 255) [   7, 107, 255, 170,   2,   7 ]
bs = P.map (\x -> P.fromIntegral x / 255) [ 100, 203, 255,   0,   0, 100 ]

-- 1. Gradient of the secant lines between each point
--
det xs ys =
  P.zipWith (/)
    (P.zipWith (-) (P.tail ys) ys)
    (P.zipWith (-) (P.tail xs) xs)

det_r = det ps rs -- (P.map P.fromIntegral rs)
det_g = det ps gs -- (P.map P.fromIntegral gs)
det_b = det ps bs -- (P.map P.fromIntegral bs)

-- 2. Tangent at every data point as the point as the average of the secants
--
m xs =
  let f x y | signum x P.== signum y = (x + y) / 2
            | P.otherwise            = 0
  in
  P.zipWith f (P.tail xs) xs

m_r = P.head det_r : m det_r
m_g = P.head det_g : m det_g
m_b = P.head det_b : m det_b

alpha_r = P.zipWith (/) m_r det_r
alpha_g = P.zipWith (/) m_g det_g
alpha_b = P.zipWith (/) m_b det_b

beta_r  = P.zipWith (/) (P.tail m_r) det_r
beta_g  = P.zipWith (/) (P.tail m_g) det_g
beta_b  = P.zipWith (/) (P.tail m_b) det_b
--}

