module TestUtils where

import Test.QuickCheck

import Width
import Number
import Expr
import RunGhc

-- | An interpreter reducing an 'Expr' to a 'Number'.
type Interpreter w =
    (KnownWidth w) => Expr w -> IO (Number w)

refInterpreter :: Interpreter w
refInterpreter = pure . interpret

ghcStaticInterpreter :: Compiler -> Interpreter W64
ghcStaticInterpreter comp e =
    fromUnsigned <$> evalGhcStatic comp e

ghcDynInterpreter' :: Compiler -> Interpreter W64
ghcDynInterpreter' comp e =
    fromUnsigned <$> evalGhcDyn comp e

-- | Do two 'Interpreter's agree in their evaluation of the given expression?
agree
    :: (KnownWidth width)
    => Interpreter width
    -> Interpreter width
    -> Expr width
    -> Property
agree interp1 interp2 e = ioProperty $ do
    r1 <- interp1 e
    r2 <- interp2 e
    return $ r1 === r2

-- | Check that an 'Interpreter' successfully evaluates an expression.
interpreterConverges
    :: (KnownWidth width)
    => Interpreter width
    -> Expr width
    -> Property
interpreterConverges interp e = ioProperty $ do
    v <- interp e
    v `seq` return True
