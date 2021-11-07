-- | Utilities for running GHC and evaluating Cmm via @run-it@.
module RunGhc
    ( Compiler(..)
    , addArgs
    , evalGhcStatic
    , evalGhcDyn
    , evalCmm
    , compile
    , runIt
    ) where

import Numeric.Natural
import System.Exit
import System.Process
import System.IO.Temp
import System.FilePath

import Expr
import Width
import ToCmm

-- | The location of GHC and arguments to pass it.
data Compiler = Compiler { compPath :: FilePath
                         , compArgs :: [String]
                         , compRunIt :: FilePath
                           -- ^ Path of the `run-it` executable built with this compiler.
                         }
    deriving (Show)

addArgs :: Compiler -> [String] -> Compiler
addArgs c args = c { compArgs = compArgs c ++ args }

-- | Compile a set of compilation units.
compile :: Compiler
        -> FilePath   -- ^ working directory
        -> [FilePath] -- ^ sources
        -> FilePath   -- ^ output path
        -> [String]   -- ^ other arguments
        -> IO ()
compile comp workDir srcs out args = do
    runProcess' $ inTmp (proc (compPath comp) allArgs)
  where
    allArgs = compArgs comp ++ srcs ++ args ++ ["-o", out]
    inTmp c = c { cwd = Just workDir }
    runProcess' p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()

-- | Evaluate an 'Expr' without relying on @run-it@. This is a bit slower than
-- 'evalGhcDyn'.
evalGhcStatic
    :: forall width. (KnownWidth width)
    => Compiler -> Expr width -> IO Natural
evalGhcStatic comp e = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> hsSrc) $ unlines
        [ "{-# LANGUAGE GHCForeignImportPrim #-}"
        , "{-# LANGUAGE UnliftedFFITypes #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "module Main where"
        , "import Data.Word"
        , "import GHC.Exts"
        , "import GHC.Ptr (Ptr(Ptr))"
        , "import System.IO.MMap"
        , "foreign import prim \"test\" test :: Addr# -> " <> hsType w
        , "main :: IO ()"
        , "main = do"
        , "  (Ptr p, _, _, _) <- mmapFilePtr \"test\" ReadOnly Nothing"
        , "  print $ " <> toHsWord w "test p"
        ]
    writeFile (tmpDir </> cmmSrc) $ toCmmDecl "test" e
    compile comp tmpDir [cmmSrc, hsSrc] exeName []
    out <- readProcess (tmpDir </> exeName) [] ""
    return $ read out
  where
    w = knownWidth @width
    exeName = "Test"
    cmmSrc = "test-cmm.cmm"
    hsSrc = "test-hs.hs"

hsType :: Width -> String
hsType W8  = "Word8#"
hsType W16 = "Word16#"
hsType W32 = "Word32#"
hsType W64 = "Word#"

toHsWord :: Width -> String -> String
toHsWord w x = "W# " <> parens (extendFn <> " " <> parens x)
  where
    extendFn
      | w == W64  = ""
      | otherwise =  "extendWord" <> show (widthBits w) <> "#"

-- | Evaluate an 'Expr'.
evalGhcDyn :: Compiler -> Expr WordSize -> IO Natural
evalGhcDyn comp e = evalCmm comp $ toCmmDecl "test" e

type Cmm = String

-- | Invoke @run-it@ on the given shared object.
runIt :: Compiler -> FilePath -> IO String
runIt comp soName =
    readProcess (compRunIt comp) [soName] ""

-- | Evaluate a Cmm function using @run-it@. The function must be named @test@
-- and must return a @bits64@.
evalCmm :: Compiler -> Cmm -> IO Natural
evalCmm comp cmm = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> cmmSrc) cmm
    let args = ["-dynamic", "-package-env", "-", "-shared"]
    compile comp tmpDir [cmmSrc] soName args
    out <- runIt comp (tmpDir </> soName)
    return $ read out
  where
    soName = "Test.so"
    cmmSrc = "test-cmm.cmm"
