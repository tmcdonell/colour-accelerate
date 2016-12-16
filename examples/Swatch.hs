{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.Names
import Data.Array.Accelerate.Interpreter                            as I

import System.FilePath
import Language.Haskell.TH                                          as TH
import Prelude                                                      as P


swatch :: Colour -> Array DIM2 Word32
swatch c
  = I.run
  $ A.map packRGB
  $ A.fill (constant (Z:.50:.200)) (constant c)

$(runQ $ do
  let
      swatches :: Q [TH.Exp]
      swatches = do
        ns <- names
        sequence [ [| writeImageToBMP $(stringE ("samples" </> "swatch" </> nameBase n <.> "bmp")) (swatch $(varE n)) |]
                 | n <- ns ]

      names :: Q [TH.Name]
      names = do
        info <- reify ''NamedColour
        case info of
          ClassI (ClassD _ _ _ _ ds) _ -> return [ n | SigD n _ <- ds ]
          _                            -> return []

  body <- swatches
  return
    [ SigD (mkName "main") (AppT (ConT (mkName "IO")) (ConT (mkName "()")))
    , FunD (mkName "main") [Clause [] (NormalB (DoE (P.map NoBindS body))) []]
    ]
 )

