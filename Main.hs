module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

prop :: KnownWidth width => Expr width -> Property
prop e =
    interpreterConverges e .&. ghcAgrees ["-O0", "-dcmm-lint", "-dasm-lint"] e

interpreterConverges :: KnownWidth width => Expr width -> Property
interpreterConverges e = property $ interpret e `seq` True

ghcAgrees :: KnownWidth width => [String] -> Expr width -> Property
ghcAgrees args e = ioProperty $ do
    r <- evalGhcDyn args e
    return $ toUnsigned (interpret e) === r

run :: forall width. (KnownWidth width) => IO Result
run = do
    createBufferFile
    quickCheckResult $ verbose (prop @width)

main :: IO ()
main = do
    _ <- run @W64
    return ()
