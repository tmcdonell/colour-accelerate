{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Data.Array.Accelerate.Data.Colour.ARGB
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- RGBA quadruples for an unspecified colour space
--

module Data.Array.Accelerate.Data.Colour.ARGB (

  Colour,
  RGBA(..),

  rgba, rgba8,
  blend,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Typeable


-- | An RGBA colour value.
--
type Colour = RGBA Float


-- | Construct an RGB colour from individual channel components. The components
-- will be clamped to the rang [0..1].
--
rgba :: Exp Float     -- ^ red component
     -> Exp Float     -- ^ green component
     -> Exp Float     -- ^ blue component
     -> Exp Float     -- ^ alpha component
     -> Exp Colour
rgba r g b a
  = clamp
  $ lift (RGBA r g b a)


-- | Construct a colour from 8-bits-per-channel colour components.
--
rgba8
    :: Exp Word8      -- ^ red component
    -> Exp Word8      -- ^ green component
    -> Exp Word8      -- ^ blue component
    -> Exp Word8      -- ^ alpha component
    -> Exp Colour
rgba8 r g b a
  = lift
  $ RGBA (A.fromIntegral r / 255)
         (A.fromIntegral g / 255)
         (A.fromIntegral b / 255)
         (A.fromIntegral a / 255)


-- | Clamp each component to the range [0..1]
--
clamp :: Exp Colour -> Exp Colour
clamp = lift1 (fmap c :: RGBA (Exp Float) -> RGBA (Exp Float))
  where
    c x = 0 `max` x `min` 1


-- | Blend two colours in the given proportions
--
blend :: Exp Float      -- ^ Proportion of first colour
      -> Exp Float      -- ^ Proportion of second colour
      -> Exp Colour     -- ^ First colour
      -> Exp Colour     -- ^ Second colour
      -> Exp Colour     -- ^ Resulting colour
blend m1 m2 c1 c2 =
  let
      RGBA r1 g1 b1 a1  = unlift c1
      RGBA r2 g2 b2 a2  = unlift c2

      -- Normalise mixing proportions to ratios.
      m12 = m1 + m2
      m1' = m1 / m12
      m2' = m2 / m12

      -- Colour components should be added via sum of squares, otherwise the
      -- result will be too dark.
      r1s = r1 * r1;    r2s = r2 * r2
      g1s = g1 * g1;    g2s = g2 * g2
      b1s = b1 * b1;    b2s = b2 * b2
  in
  rgba (sqrt (m1' * r1s + m2' * r2s))
       (sqrt (m1' * g1s + m2' * g2s))
       (sqrt (m1' * b1s + m2' * b2s))
       ((m1 * a1 + m2 * a2) / m12)


-- Accelerate bits
-- ---------------

-- | An RGBA colour value to hold the colour components. All components lie in
-- the range [0..1).
--
-- We need to parameterise by a type so that we can have both Exp (RGBA a) and
-- RGBA (Exp a).
--
data RGBA a = RGBA a a a a
  deriving (Show, Eq, Functor, Typeable)

-- Represent colours in Accelerate as a 4-tuple
--
type instance EltRepr (RGBA a) = EltRepr (a, a, a, a)

instance Elt a => Elt (RGBA a) where
  eltType (_ :: RGBA a)         = eltType (undefined :: (a,a,a,a))
  toElt c                       = let (r,g,b,a) = toElt c in RGBA r g b a
  fromElt (RGBA r g b a)        = fromElt (r,g,b,a)

instance Elt a => IsProduct Elt (RGBA a) where
  type ProdRepr (RGBA a)         = (((((),a), a), a), a)
  fromProd _ (RGBA r g b a)      = (((((), r), g), b), a)
  toProd _ (((((),r),g),b),a)    = RGBA r g b a
  prod cst _                     = prod cst (undefined :: (a,a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGBA a) where
  type Plain (RGBA a)   = RGBA (Plain a)
  lift (RGBA r g b a)   = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g
                                               `SnocTup` lift b `SnocTup` lift a

instance Elt a => Unlift Exp (RGBA (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` c
                      g = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      b = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      a = Exp $ ZeroTupIdx `Prj` c
                  in RGBA r g b a

{--
instance Num a => Num (RGBA a) where
  (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1

  (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1

  (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1

  abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1

  signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1

  fromInteger i
        = let f = fromInteger i
          in  RGBA f f f 1

instance (Elt a, IsNum a) => Num (Exp (RGBA a)) where
  (+)           = lift2 ((+) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (-)           = lift2 ((-) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  (*)           = lift2 ((*) :: RGBA (Exp a) -> RGBA (Exp a) -> RGBA (Exp a))
  abs           = lift1 (abs :: RGBA (Exp a) -> RGBA (Exp a))
  signum        = lift1 (signum :: RGBA (Exp a) -> RGBA (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                      a = constant 1
                  in lift $ RGBA f f f a
--}

