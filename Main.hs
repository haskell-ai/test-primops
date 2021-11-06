module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

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

ghcDynInterpreter :: Interpreter W64
ghcDynInterpreter e =
    fromUnsigned <$> evalGhcDyn ghcArgs e
  where
    ghcArgs = ["-O0", "-dcmm-lint", "-dasm-lint"]

prop :: KnownWidth W64 => Expr W64 -> Property
prop e = conjoin
    [ interpreterConverges e
    , agree refInterpreter ghcDynInterpreter e
    ]

interpreterConverges :: KnownWidth width => Expr width -> Property
interpreterConverges e =
    property $ interpret e `seq` True

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

quotRemProp :: (KnownWidth width)
           => Interpreter WordSize
           -> Signedness
           -> Number width            -- ^ dividend
           -> NonZero (Number width)  -- ^ divisor
           -> Property
quotRemProp interp s a (NonZero b) = ioProperty $ do
    r <- interp (ERel REq x rhs)
    return $ r === 1
  where
    x = ELit a
    y = ELit b
    rhs = ((EQuot s x y) * y) + ERem s x y

run :: forall width. (KnownWidth width) => IO Result
run = do
    createBufferFile
    quickCheckResult $ verbose prop

main :: IO ()
main = do
    _ <- run @W64
    return ()
