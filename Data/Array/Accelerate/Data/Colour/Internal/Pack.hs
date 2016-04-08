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
-- will be the highest byte in the word, and the last argument the lowest. (Thus
-- on a little-endian architecture, the first argument appears first in memory.)
--
pack8 :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word32
pack8 x y z w =
      A.fromIntegral x `A.shiftL` 24
  .|. A.fromIntegral y `A.shiftL` 16
  .|. A.fromIntegral z `A.shiftL` 8
  .|. A.fromIntegral w

-- | Inverse of 'pack'.
--
unpack8 :: Exp Word32 -> Exp (Word8, Word8, Word8, Word8)
unpack8 xyzw =
  let x = A.fromIntegral (xyzw `A.shiftR` 24)
      y = A.fromIntegral (xyzw `A.shiftR` 16)
      z = A.fromIntegral (xyzw `A.shiftR` 8)
      w = A.fromIntegral xyzw
  in
  lift (x,y,z,w)


word8OfFloat :: Exp Float -> Exp Word8
word8OfFloat x = A.truncate (x * 255)


