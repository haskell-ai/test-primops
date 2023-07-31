{-# LANGUAGE CPP #-}

-- | Utilities for running GHC and evaluating Cmm via @run-it@.
module RunGhc
    ( EvalMethod(..)
    , staticEvalMethod
    , emulatedStaticEvalMethod
    , runTestProgram
      -- * Evaluating expressions
    , evalExpr
      -- * Evaluating Cmm programs
    , evalCmm
      -- * Utilities
    , dumpCmmAsm
    , dumpExprAsm
    ) where

import Numeric.Natural
import System.Process
import System.IO.Temp
import System.FilePath

import Compiler
import Expr
import Width
import ToCmm

data EvalMethod
    = StaticEval { compiler :: Compiler
                 , runExe :: FilePath -> IO String
                 }
    | DynamicEval { compiler :: Compiler
                  , runItPath :: FilePath
                  }

staticEvalMethod :: Compiler -> EvalMethod
staticEvalMethod comp =
    StaticEval comp runExe
  where
    runExe exe = readProcess exe [] ""

-- | An 'EvalMethod' using an emulator to run target executables.
emulatedStaticEvalMethod :: Compiler -> FilePath -> EvalMethod
emulatedStaticEvalMethod comp emulator =
    StaticEval comp runExe
  where
    runExe exe = readProcess emulator [exe] ""

runTestProgram :: EvalMethod -> Width -> TestProgram -> IO String
runTestProgram (StaticEval comp runExe) = runTestProgramStatic comp runExe
runTestProgram (DynamicEval comp runIt) = runTestProgramDyn comp runIt

runTestProgramDyn :: Compiler -> FilePath
                  -> Width -> TestProgram
                  -> IO String
runTestProgramDyn comp runItPath width tp =
    withTempDirectory "." "tmp" $ \tmpDir -> do
        objs <- writeObjectsIn tmpDir tp
        compile comp tmpDir objs soName args
        readProcess runItPath [show (widthBits width), tmpDir </> soName] ""
  where
    args = ["-dynamic", "-package-env", "-", "-shared"]
    soName = "Test.so"

runTestProgramStatic :: Compiler -> (FilePath -> IO String)
                     -> Width -> TestProgram
                     -> IO String
runTestProgramStatic comp runExe width tp =
    withTempDirectory "." "tmp" $ \tmpDir -> do
        wrapper <- mkStaticWrapper comp width
        objs <- writeObjectsIn tmpDir (tp <> wrapper)
        compile comp tmpDir objs exeName ["-package", "bytestring"]
        runExe (tmpDir </> exeName)
  where
    exeName = "Test"

mkStaticWrapper
    :: Compiler
    -> Width
    -> IO TestProgram
mkStaticWrapper comp width = do
    compileHs comp src
  where
    src = unlines
        [ "{-# LANGUAGE GHCForeignImportPrim #-}"
        , "{-# LANGUAGE UnliftedFFITypes #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "module Main where"
        , "import GHC.Word"
        , "import GHC.Exts"
        , "import GHC.Ptr (Ptr(Ptr))"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BS"
        , "foreign import prim \"test\" test :: Addr# -> " <> hsWordType width
        , "main :: IO ()"
        , "main = do"
        , "  buf <- BS.readFile \"test\""
        , "  BS.unsafeUseAsCString buf $ \\(Ptr p) -> do"
        , "    let res = " <> hsWordCon width <> " (test p)"
        , "    print res"
        ]

-- | Expects the 'Cmm' to be a function named @test@ which returns a
-- @width@-size integer.
evalCmm :: EvalMethod -> Width -> Cmm -> IO Natural
evalCmm em width cmm = do
    tp <- compileCmm (compiler em) cmm
    out <- runTestProgram em width tp
    return $ read out

-- | Evaluate an 'Expr'.
evalExpr :: forall width. KnownWidth width
         => EvalMethod -> Expr width -> IO Natural
evalExpr em = evalCmm em (knownWidth @width) . toCmmDecl "test"

type Cmm = String

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
