{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Data.Array.Accelerate.Data.Colour.RGB
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- RGB triples for an unspecified colour space
--

module Data.Array.Accelerate.Data.Colour.RGB (

  Colour,
  RGB(..),

  rgb, rgb8,
  blend

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Typeable


-- | An RGB colour value
--
type Colour = RGB Float

-- | Construct an RGB colour from individual channel components. The components
-- will be clamped to the range [0..1].
--
rgb :: Exp Float -> Exp Float -> Exp Float -> Exp Colour
rgb r g b
  = clamp
  $ lift (RGB r g b)


-- | Construct a colour from 8-bit-per-channel colour components.
--
rgb8 :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Colour
rgb8 r g b
  = lift
  $ RGB (A.fromIntegral r / 255)
        (A.fromIntegral g / 255)
        (A.fromIntegral b / 255)


-- | Clamp each component of a colour to the range [0..1].
--
clamp :: Exp Colour -> Exp Colour
clamp = lift1 (fmap c :: RGB (Exp Float) -> RGB (Exp Float))
  where
    c x = 0 `max` x `min` 1


-- | Blend two colours in the given proportions
--
blend :: Exp Float      -- ^ proportion of first colour
      -> Exp Float      -- ^ proportion of second colour
      -> Exp Colour     -- ^ first colour
      -> Exp Colour     -- ^ second colour
      -> Exp Colour
blend m1 m2 c1 c2 =
  let
      RGB r1 g1 b1    = unlift c1
      RGB r2 g2 b2    = unlift c2

      -- Normalise mixing proportions to ratios.
      m12 = m1 + m2
      m1' = m1 / m12
      m2' = m2 / m12

      -- Colours components should be added via sum of squares, otherwise the
      -- result will be too dark.
      r1s = r1 * r1;    r2s = r2 * r2
      g1s = g1 * g1;    g2s = g2 * g2
      b1s = b1 * b1;    b2s = b2 * b2
  in
  rgb (sqrt (m1' * r1s + m2' * r2s))
      (sqrt (m1' * g1s + m2' * g2s))
      (sqrt (m1' * b1s + m2' * b2s))


-- Accelerate bits
-- ---------------

-- RGB colour space
--
data RGB a = RGB a a a
  deriving (Show, Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 3-tuple
--
type instance EltRepr (RGB a) = EltRepr (a, a, a)

instance Elt a => Elt (RGB a) where
  eltType (_ :: RGB a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (r,g,b) = toElt c in RGB r g b
  fromElt (RGB r g b)           = fromElt (r,g,b)

instance Elt a => IsProduct Elt (RGB a) where
  type ProdRepr (RGB a)          = ((((),a), a), a)
  fromProd _ (RGB r g b)         = ((((), r), g), b)
  toProd _ ((((),r),g),b)        = RGB r g b
  prod cst _                     = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGB a) where
  type Plain (RGB a)    = RGB (Plain a)
  lift (RGB r g b)      = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g `SnocTup` lift b

instance Elt a => Unlift Exp (RGB (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      g = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      b = Exp $ ZeroTupIdx `Prj` c
                  in RGB r g b

{--
instance Num a => Num (RGB a) where
  (+) (RGB r1 g1 b1 ) (RGB r2 g2 b2)
        = RGB (r1 + r2) (g1 + g2) (b1 + b2)

  (-) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 - r2) (g1 - g2) (b1 - b2)

  (*) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 * r2) (g1 * g2) (b1 * b2)

  abs (RGB r1 g1 b1)
        = RGB (abs r1) (abs g1) (abs b1)

  signum (RGB r1 g1 b1)
        = RGB (signum r1) (signum g1) (signum b1)

  fromInteger i
        = let f = fromInteger i
          in  RGB f f f


instance (Elt a, IsNum a) => Num (Exp (RGB a)) where
  (+)           = lift2 ((+) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (-)           = lift2 ((-) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (*)           = lift2 ((*) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  abs           = lift1 (abs :: RGB (Exp a) -> RGB (Exp a))
  signum        = lift1 (signum :: RGB (Exp a) -> RGB (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                  in lift $ RGB f f f
--}

