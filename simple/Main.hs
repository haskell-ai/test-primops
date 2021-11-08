{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.Ptr
import System.IO.MMap
import Foreign.Marshal.Alloc

foreign import prim "test" test :: Addr# -> Word#

main :: IO ()
main = do
  (Ptr p, _, _, _) <- mmapFilePtr "test" ReadOnly Nothing
  --Ptr p <- callocBytes (1*1000*1000)
  print $ W# (test p)

