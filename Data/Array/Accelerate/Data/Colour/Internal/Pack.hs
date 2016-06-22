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
import Data.Array.Accelerate.Data.Bits          as A


-- | Pack the given four bytes into a single 4-byte word. The first argument
-- will appear at the lowest address on a little-endian architecture with
-- one-byte addressing:
--
-- >>> pack8 0x0d 0x0c 0x0b 0x0a = 0x0a0b0c0d
--
-- This function is equivalent to:
--
-- >>> alloca $ \(p :: Ptr Word32) ->
-- >>>   pokeByteOff p 0 (0x0d :: Word8)
-- >>>   pokeByteOff p 1 (0x0c :: Word8)
-- >>>   pokeByteOff p 2 (0x0b :: Word8)
-- >>>   pokeByteOff p 3 (0x0a :: Word8)
--
-- Where 'p' would then point to the value '0x0a0b0c0d'
--
pack8 :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word32
pack8 x y z w =
      A.fromIntegral w `A.shiftL` 24
  .|. A.fromIntegral z `A.shiftL` 16
  .|. A.fromIntegral y `A.shiftL` 8
  .|. A.fromIntegral x

-- | Inverse of 'pack'. On a little-endian architecture:
--
-- >>> unpack8 0x0a0b0c0d = (0x0d, 0x0c, 0x0b, 0x0a)
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

