module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm
import TestUtils

prop :: KnownWidth W64 => Expr W64 -> Property
prop e = conjoin
    [ interpreterConverges refInterpreter e
    , agree refInterpreter ghcDynInterpreter e
    ]

divAgrees =
    verbose $ \s x (NonZero y) -> 
        agree refInterpreter ghcDynInterpreter $ EQuot @W64 s (ELit x) (ELit y)

quotRemProp
    :: (KnownWidth width)
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
