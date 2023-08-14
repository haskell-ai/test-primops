{-# LANGUAGE CPP #-}

-- | Utilities for running GHC and evaluating Cmm via @run-it@.
module RunGhc
    ( ProcessResult(..)
    , ProcessFailure(..)
    , throwFailure
    , EvalMethod(..)
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

import Control.Exception
import Numeric.Natural
import System.Process
import System.IO.Temp
import System.Exit
import System.FilePath

import Compiler
import Expr
import Width
import ToCmm

data ProcessResult
    = ProcessSucceeded { prStdout :: String
                       , prStderr :: String
                       }
    | ProcessFailed ProcessFailure

data ProcessFailure = ProcessFailure { preExitCode :: Int }
    deriving (Show, Eq)

instance Exception ProcessFailure

throwFailure :: IO (Either ProcessFailure a) -> IO a
throwFailure = (>>= either throwIO return)

data EvalMethod
    = StaticEval { compiler :: Compiler
                 , runExe :: FilePath -> IO ProcessResult
                 }
    | DynamicEval { compiler :: Compiler
                  , runItPath :: FilePath
                  }

readProcess' :: FilePath -- ^ executable
             -> [String] -- ^ arguments
             -> String   -- ^ stdin
             -> IO ProcessResult
readProcess' exe args stdin = do
    (code, out, err) <- readProcessWithExitCode exe args stdin
    case code of
      ExitSuccess   -> return $ ProcessSucceeded out err
      ExitFailure n -> return $ ProcessFailed $ ProcessFailure n

staticEvalMethod :: Compiler -> EvalMethod
staticEvalMethod comp =
    StaticEval comp runExe
  where
    runExe exe = readProcess' exe [] ""

-- | An 'EvalMethod' using an emulator to run target executables.
emulatedStaticEvalMethod :: Compiler -> FilePath -> EvalMethod
emulatedStaticEvalMethod comp emulator =
    StaticEval comp runExe
  where
    runExe exe = readProcess' emulator [exe] ""

runTestProgram :: EvalMethod -> Width -> TestProgram -> IO ProcessResult
runTestProgram (StaticEval comp runExe) = runTestProgramStatic comp runExe
runTestProgram (DynamicEval comp runIt) = runTestProgramDyn comp runIt

runTestProgramDyn :: Compiler -> FilePath
                  -> Width -> TestProgram
                  -> IO ProcessResult
runTestProgramDyn comp runItPath width tp =
    withTempDirectory "." "tmp" $ \tmpDir -> do
        objs <- writeObjectsIn tmpDir tp
        compile comp tmpDir objs soName args
        readProcess' runItPath [show (widthBits width), tmpDir </> soName] ""
  where
    args = ["-dynamic", "-package-env", "-", "-shared"]
    soName = "Test.so"

runTestProgramStatic :: Compiler
                     -> (FilePath -> IO ProcessResult)
                     -> Width -> TestProgram
                     -> IO ProcessResult
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
evalCmm :: EvalMethod -> Width -> Cmm -> IO (Either ProcessFailure Natural)
evalCmm em width cmm = do
    tp <- compileCmm (compiler em) cmm
    out <- runTestProgram em width tp
    case out of
      ProcessSucceeded out _err -> return $ Right $ read out
      ProcessFailed failure -> return $ Left failure

-- | Evaluate an 'Expr'.
evalExpr :: forall width. KnownWidth width
         => EvalMethod -> Expr width -> IO (Either ProcessFailure Natural)
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
