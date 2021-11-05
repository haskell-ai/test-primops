module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

prop :: KnownWidth width => Expr width -> Property
prop e = interpreterConverges e .&. ghcAgrees e

interpreterConverges :: KnownWidth width => Expr width -> Property
interpreterConverges e = property $ interpret e `seq` True

ghcAgrees :: KnownWidth width => Expr width -> Property
ghcAgrees e = ioProperty $ do
    r <- evalGhc e
    return $ getNumber (interpret e) === r

main :: IO ()
main = do
    createBufferFile
    quickCheck $ verbose (prop @W64)
