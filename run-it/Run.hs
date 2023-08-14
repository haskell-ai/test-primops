{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.Ptr
import GHC.Word
import System.Posix (RTLDFlags(..), dlopen, dlsym, dlclose)
import System.Environment
import System.IO.MMap

foreign import prim "stg_trampoline" trampoline8  :: Addr# -> Addr# -> Word8#
foreign import prim "stg_trampoline" trampoline16 :: Addr# -> Addr# -> Word16#
foreign import prim "stg_trampoline" trampoline32 :: Addr# -> Addr# -> Word32#
foreign import prim "stg_trampoline" trampoline64 :: Addr# -> Addr# -> Word64#

usage :: IO a
usage = fail $ unlines
    [ "usage: run-it <buffer> <width> <file.so>"
    , "where"
    , "  <buffer> is a file to serve as the test buffer"
    , "  <width> is one of 8, 16, 32, 64"
    , "  <file.so> is a shared object containing a `test` symbol"
    ]

main :: IO ()
main = do
  args <- getArgs
  (buffer, width, so) <- case args of
                   [buffer, width, so] -> pure (buffer, width, so)
                   _ -> usage

  (Ptr p, _, _, _) <- mmapFilePtr buffer ReadOnly Nothing
  dl <- dlopen so [RTLD_NOW]
  Ptr entry <- castFunPtrToPtr <$> dlsym dl "test"
  case width of
    "8"  -> print $ W8# (trampoline8 entry p)
    "16" -> print $ W16# (trampoline16 entry p)
    "32" -> print $ W32# (trampoline32 entry p)
    "64" -> print $ W64# (trampoline64 entry p)
    _    -> usage
  dlclose dl
