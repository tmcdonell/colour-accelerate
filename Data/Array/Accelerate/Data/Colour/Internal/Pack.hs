-- |
-- Module      : Data.Array.Accelerate.Data.Colour.Internal.Pack
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.Colour.Internal.Pack
  where

import Data.Array.Accelerate                    as A
import Data.Bits


-- | Pack the given four bytes into a single 4-byte word. The first argument
-- will appear in-memory as the first byte on a little-endian architecture.
--
pack8 :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word32
pack8 x y z w =
      A.fromIntegral w `A.shiftL` 24
  .|. A.fromIntegral z `A.shiftL` 16
  .|. A.fromIntegral y `A.shiftL` 8
  .|. A.fromIntegral x

-- | Inverse of 'pack'.
--
unpack8 :: Exp Word32 -> Exp (Word8, Word8, Word8, Word8)
unpack8 xyzw =
  let w = A.fromIntegral (xyzw `A.shiftR` 24)
      z = A.fromIntegral (xyzw `A.shiftR` 16)
      y = A.fromIntegral (xyzw `A.shiftR` 8)
      x = A.fromIntegral xyzw
  in
  lift (x,y,z,w)


word8OfFloat :: Exp Float -> Exp Word8
word8OfFloat x = A.truncate (x * 255)

