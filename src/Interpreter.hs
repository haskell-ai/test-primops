module Interpreter
    ( Interpreter
    , refInterpreter
    , ghcStaticInterpreter
    , ghcCrossInterpreter
    , ghcDynInterpreter
      -- * Properties of interpreters
    , agree
    , converges
    ) where

import Test.QuickCheck
import System.Process

import ToCmm
import Width
import Number
import Expr
import RunGhc

-- | An interpreter reducing an 'Expr' to a 'Number'.
type Interpreter w =
    (KnownWidth w) => Expr w -> IO (Number w)

refInterpreter :: Interpreter w
refInterpreter = pure . interpret

-- | An 'Interpreter' which compiles the given expression into a test
-- executable and run it.
ghcStaticInterpreter :: Compiler -> Interpreter WordSize
ghcStaticInterpreter comp e =
    let run exe = readProcess exe [] ""
     in ghcStaticInterpreter' comp run e

-- | An 'Interpreter' which compiles the given expression into a test
-- executable and run it using the provided emulator.
ghcCrossInterpreter :: Compiler  -- ^ a cross-compiler
                    -> FilePath  -- ^ emulator for executing compiled executables
                    -> Interpreter WordSize
ghcCrossInterpreter comp emulator e =
    let run exe = readProcess emulator [exe] ""
     in ghcStaticInterpreter' comp run e

ghcStaticInterpreter' :: Compiler -> (FilePath -> IO String) -> Interpreter WordSize
ghcStaticInterpreter' comp run e =
    fromUnsigned <$> evalGhcStatic comp run e

-- | An 'Interpreter' which compiles the given expression into a test
-- dynamic object and executes it using the @run-it@ executable.
ghcDynInterpreter :: Compiler -> RunIt -> Interpreter WordSize
ghcDynInterpreter comp runIt e =
    fromUnsigned <$> evalGhcDyn comp runIt e

-- | Do two 'Interpreter's agree in their evaluation of the given expression?
agree
    :: (KnownWidth width)
    => Interpreter width
    -> Interpreter width
    -> Expr width
    -> Property
agree interp1 interp2 e = counterexample (exprToCmm e) $ ioProperty $ do
    r1 <- interp1 e
    r2 <- interp2 e
    return $ r1 === r2

-- | Check that an 'Interpreter' successfully evaluates an expression.
converges
    :: (KnownWidth width)
    => Interpreter width
    -> Expr width
    -> Property
converges interp e = ioProperty $ do
    v <- interp e
    v `seq` return True
