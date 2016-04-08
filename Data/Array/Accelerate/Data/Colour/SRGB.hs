{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Colour.SRGB
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Colours in the sRGB standard.
--

module Data.Array.Accelerate.Data.Colour.SRGB (

  Colour,
  SRGB,
  srgb, srgb8,
  toRGB, fromRGB,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Colour.RGB    ( RGB(..) )


-- | An sRGB colour value
--
type Colour = SRGB Float

-- | Synonym for an RGB colour that is in the sRGB colour space.
--
type SRGB a = RGB a

-- | Construct an sRGB colour from individual channel components. The components
-- will be clamped to the range [0..1].
--
srgb :: Exp Float       -- ^ red component
     -> Exp Float       -- ^ green component
     -> Exp Float       -- ^ blue component
     -> Exp Colour
srgb r g b
  = clamp
  $ lift (RGB r g b)


-- | Construct an sRGB colour from 8-bit-per-channel colour components.
--
srgb8 :: Exp Word8      -- ^ red component
      -> Exp Word8      -- ^ green component
      -> Exp Word8      -- ^ blue component
      -> Exp Colour
srgb8 r g b
  = lift
  $ RGB (A.fromIntegral r / 255)
        (A.fromIntegral g / 255)
        (A.fromIntegral b / 255)


-- | Clamp each component of a colour to the range [0..1].
--
clamp :: Exp Colour -> Exp Colour
clamp = lift1 (fmap c :: SRGB (Exp Float) -> SRGB (Exp Float))
  where
    c x = 0 `max` x `min` 1


-- | Convert a colour in the sRGB colour space into the linear RGB colour space.
--
toRGB :: Exp (SRGB Float) -> Exp (RGB Float)
toRGB (unlift -> RGB r g b)
  = lift
  $ RGB (invTransferFunction r)
        (invTransferFunction g)
        (invTransferFunction b)

-- | Convert a colour in the linear RGB colour space into the non-linear SRGB
-- colour space.
--
fromRGB :: Exp (RGB Float) -> Exp (SRGB Float)
fromRGB (unlift -> RGB r g b)
  = lift
  $ RGB (transferFunction r)
        (transferFunction g)
        (transferFunction b)


-- The non-linear sRGB transfer function approximates a gamma of about 2.2.
--
transferFunction :: Exp Float -> Exp Float
transferFunction lin
  = lin ==* 1         ? ( 1
  , lin <=* 0.0031308 ? ( 12.92 * lin
  , {- otherwise -}       let a = 0.055
                          in (1 + a)*lin**(1/2.4) - a ))

invTransferFunction :: Exp Float -> Exp Float
invTransferFunction nonlin
  = nonlin ==* 1       ? ( 1
  , nonlin <=* 0.04045 ? ( nonlin/12.92
  , {- otherwise -}        let a = 0.055
                           in ((nonlin + a)/(1 + a))**2.4 ))

