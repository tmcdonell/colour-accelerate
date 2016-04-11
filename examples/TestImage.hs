
import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Interpreter

import Data.Array.Accelerate.IO
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.Names


zoom :: Int
zoom = 25

test :: Acc (Array DIM2 (RGB Word8))
test = A.generate (constant (Z :. 19*zoom :. 12*zoom)) go
  where
    go :: Exp DIM2 -> Exp (RGB Word8)
    go ix =
      let Z :. y :. x = unlift ix
          j           = y `div` constant zoom
          i           = x `div` constant zoom
      in
        -- by row
        j ==*  0 ? ( constant black
      , j ==*  3 ? ( constant magenta
      , j ==*  6 ? ( constant cyan
      , j ==*  9 ? ( constant yellow
      , j ==* 12 ? ( constant blue
      , j ==* 15 ? ( constant red
      , j ==* 18 ? ( constant white
        -- otherwise by column
      , i <=*  1 ? ( constant red
      , i <=*  3 ? ( constant green
      , i <=*  5 ? ( constant blue
      , i <=*  7 ? ( constant yellow
      , i <=*  9 ? ( constant cyan
      , i <=* 11 ? ( constant magenta
      , {- otherwise -} constant black )))))))))))))


main :: IO ()
main
  = writeImageToBMP "blocks.bmp"
  $ run (A.map packRGB8 test)

