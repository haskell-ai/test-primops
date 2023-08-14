module Interpreter
    ( Interpreter
    , refInterpreter
    , ghcInterpreter
      -- * Properties of interpreters
    , agree
    , converges
    ) where

import Control.Exception
import Test.QuickCheck

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

-- | An 'Interpreter' which compiles and evaluates the given expression.
-- May throw 'ProcessFailure' when test program diverges.
ghcInterpreter :: EvalMethod -> Interpreter WordSize
ghcInterpreter em e =
    fromUnsigned <$> throwFailure (evalExpr em e)

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
