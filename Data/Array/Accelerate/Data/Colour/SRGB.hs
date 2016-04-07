{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

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

  SRGB,
  srgb, toRGB, fromRGB,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Colour.RGB


-- | Synonym for an RGB colour that is in the sRGB colour space.
--
type SRGB a = RGB a

-- | Construct an sRGB colour from individual sRGB channel components
--
srgb :: Elt a => Exp a -> Exp a -> Exp a -> Exp (SRGB a)
srgb r g b = lift (RGB r g b)


-- | Convert a colour in the sRGB colour space into the linear RGB colour space.
--
toRGB :: (Elt a, IsFloating a) => Exp (SRGB a) -> Exp (RGB a)
toRGB (unlift -> RGB r g b)
  = lift
  $ RGB (invTransferFunction r)
        (invTransferFunction g)
        (invTransferFunction b)

-- | Convert a colour in the linear RGB colour space into the non-linear SRGB
-- colour space.
--
fromRGB :: (Elt a, IsFloating a) => Exp (RGB a) -> Exp (SRGB a)
fromRGB (unlift -> RGB r g b)
  = lift
  $ RGB (transferFunction r)
        (transferFunction g)
        (transferFunction b)


-- The non-linear sRGB transfer function approximates a gamma of about 2.2.
--
transferFunction :: (Elt a, IsFloating a) => Exp a -> Exp a
transferFunction lin
  = lin ==* 1         ? ( 1
  , lin <=* 0.0031308 ? ( 12.92 * lin
  , {- otherwise -}       let a = 0.055
                          in (1 + a)*lin**(1/2.4) - a ))

invTransferFunction :: (Elt a, IsFloating a) => Exp a -> Exp a
invTransferFunction nonlin
  = nonlin ==* 1       ? ( 1
  , nonlin <=* 0.04045 ? ( nonlin/12.92
  , {- otherwise -}        let a = 0.055
                           in ((nonlin + a)/(1 + a))**2.4 ))

