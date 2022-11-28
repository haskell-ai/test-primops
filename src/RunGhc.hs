-- | Utilities for running GHC and evaluating Cmm via @run-it@.
module RunGhc
    ( Compiler(..)
    , addArgs
    , evalGhcStatic
    , evalGhcDyn
    , evalCmm
    , compile
    , dumpCmmAsm
    , dumpExprAsm
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
    => Compiler                -- ^ How to compile the test executable
    -> (FilePath -> IO String) -- ^ How to run the test executable, reading stdin
    -> Expr width              -- ^ The expression to evaluate
    -> IO Natural
evalGhcStatic comp runExe =
    evalCmmStatic comp runExe . toCmmDecl "test"

evalCmmStatic :: Compiler
              -> (FilePath -> IO String)
              -> Cmm
              -> IO Natural
evalCmmStatic comp runExe cmm = withTempDirectory "." "tmp" $ \tmpDir -> do
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
    writeFile (tmpDir </> cmmSrc) cmm
    compile comp tmpDir [cmmSrc, hsSrc] exeName []
    out <- runExe (tmpDir </> exeName)
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

-- | Path to the @run-it@ executable.
newtype RunIt = RunIt FilePath

-- | Evaluate an 'Expr'.
evalGhcDyn :: Compiler -> RunIt -> Expr WordSize -> IO Natural
evalGhcDyn comp runIt = evalCmmDyn comp runIt . toCmmDecl "test"

type Cmm = String

-- | Evaluate a Cmm function using @run-it@. The function must be named @test@
-- and must return a @bits64@.
evalCmmDyn :: Compiler -> RunIt -> Cmm -> IO Natural
evalCmmDyn comp (RunIt runItPath) cmm = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> cmmSrc) cmm
    let args = ["-dynamic", "-package-env", "-", "-shared"]
    compile comp tmpDir [cmmSrc] soName args
    out <- readProcess runItPath [tmpDir </> soName] ""
    return $ read out
  where
    soName = "Test.so"
    cmmSrc = "test-cmm.cmm"

-- | Compile the given Cmm procedure and dump its disassembly.
dumpCmmAsm :: Compiler -> Cmm -> IO String
dumpCmmAsm comp cmm = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> cmmSrc) cmm
    let args = ["-S", "-dynamic", "-package-env", "-"]
    compile comp tmpDir [cmmSrc] objName args
    readFile (tmpDir </> "test-cmm.s")
  where
    objName = "test-cmm.s"
    cmmSrc = "test-cmm.cmm"

-- | Compile the given 'Expr' and dump its disassembly.
dumpExprAsm :: (KnownWidth w)
            => Compiler -> Expr w -> IO String
dumpExprAsm comp = dumpCmmAsm comp . toCmmDecl "test"
