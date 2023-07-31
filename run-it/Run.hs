{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.Ptr
import System.Posix (RTLDFlags(..), dlopen, dlsym, dlclose)
import System.Environment
import System.IO.MMap

foreign import prim "stg_trampoline" trampoline :: Addr# -> Addr# -> Word#

main :: IO ()
main = do
  [so] <- getArgs
  (Ptr p, _, _, _) <- mmapFilePtr "test" ReadOnly Nothing
  dl <- dlopen so [RTLD_NOW]
  Ptr entry <- castFunPtrToPtr <$> dlsym dl "test"
  print $ W# (trampoline entry p)
  dlclose dl
