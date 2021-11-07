module TestUtils where

import Test.QuickCheck

import Width
import Number
import Expr
import ToCmm

type Interpreter w = (KnownWidth w) => Expr w -> IO (Number w)

refInterpreter :: Interpreter w
refInterpreter = pure . interpret

ghcInterpreter :: Interpreter W64
ghcInterpreter e =
    fromUnsigned <$> evalGhc ghcArgs e
  where
    ghcArgs = ["-O0", "-dcmm-lint", "-dasm-lint"]

ghcDynInterpreter' :: [String] -> Interpreter W64
ghcDynInterpreter' ghcArgs e =
    fromUnsigned <$> evalGhcDyn ghcArgs e

ghcDynInterpreter :: Interpreter W64
ghcDynInterpreter = ghcDynInterpreter' ["-O0", "-dcmm-lint", "-dasm-lint"]

ghcDynLlvmInterpreter :: Interpreter W64
ghcDynLlvmInterpreter = ghcDynInterpreter' ["-O0", "-dcmm-lint", "-dasm-lint", "-fllvm"]

-- | Do two interpreters agree in their evaluation of the given expression?
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

interpreterConverges
    :: (KnownWidth width)
    => Interpreter width
    -> Expr width
    -> Property
interpreterConverges interp e = ioProperty $ do
    v <- interp e
    v `seq` return True
