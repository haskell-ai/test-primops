
module Main where

import Data.Bits as Bits
import Test.QuickCheck hiding ((.&.))
import Data.Proxy
import Prelude hiding (truncate)

import Width
import Number
import Expr

prop :: Gen Property
prop = do
    e <- oneof
        [ genExpr (Proxy @W8)
        ]
    return $ interpret e === interpret e

main :: IO ()
main = quickCheck prop
