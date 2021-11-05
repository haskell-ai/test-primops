module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm

type WordSize = W64

prop :: Property
prop = property $ do
    SomeExpr e <- withArbitraryWidth genExpr
    return $ ioProperty $ do
        r <- evalGhc e
        return $ getNumber (interpret e) === r

main :: IO ()
main = quickCheck prop
