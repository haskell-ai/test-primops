module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

type WordSize = W64

prop :: SomeExpr -> Property
prop (SomeExpr e) =
    ioProperty $ do
        r <- evalGhc e
        return $ getNumber (interpret e) === r

main :: IO ()
main = quickCheck prop
