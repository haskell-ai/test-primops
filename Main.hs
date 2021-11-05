module Main where

import System.Exit
import System.Process
import Test.QuickCheck
import System.Directory
import System.IO.Temp
import System.FilePath
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

type WordSize = W64

ghcPath :: FilePath
ghcPath = "ghc"

evalGhc :: KnownWidth width => Expr width -> IO Integer
evalGhc e = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> hsSrc) $ unlines
        [ "{-# LANGUAGE GHCForeignImportPrim #-}"
        , "{-# LANGUAGE UnliftedFFITypes #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "module Main where"
        , "import Data.Word"
        , "import GHC.Exts"
        , "foreign import prim \"test\" test :: Word# -> Word#"
        , "main :: IO ()"
        , "main = print (W# (test 0##))"
        ]
    writeFile (tmpDir </> cmmSrc) $ toCmmDecl "test" e
    let inTmp c = c { cwd = Just tmpDir }
    runProcess $ inTmp (proc ghcPath [hsSrc, cmmSrc, "-v0", "-o", exeName])
    out <- readProcess (tmpDir </> exeName) [] ""
    return $ read out
  where
    runProcess p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()
    exeName = "Test"
    cmmSrc = "test-cmm.cmm"
    hsSrc = "test-hs.hs"

prop :: Property
prop = property $ do
    SomeExpr e <- withArbitraryWidth genExpr
    return $ interpret e === interpret e

main :: IO ()
main = quickCheck prop
