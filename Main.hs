module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

prop :: KnownWidth width => Expr width -> Property
prop e =
    ioProperty $ do
        r <- evalGhc e
        return $ getNumber (interpret e) === r

main :: IO ()
main = quickCheck (prop @W64)
