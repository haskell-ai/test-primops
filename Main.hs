module Main where

import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr

prop :: Property
prop = property $ do
    SomeExpr e <- withArbitraryWidth genExpr
    return $ interpret e === interpret e

main :: IO ()
main = quickCheck prop
