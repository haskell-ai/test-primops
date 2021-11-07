module CallishOp where

import Data.Bits
import Test.QuickCheck

import Width
import ToCmm
import Number
import Expr

data Callish r where
    Popcount :: forall w. (KnownWidth w) => Expr w -> Callish (Number WordSize)


prop_popcount_correct
    :: forall w. (KnownWidth w)
    => Expr w -> Property
prop_popcount_correct e = ioProperty $ do
    r <- evalCmm ["-dcmm-lint"] cmm
    let r' = popCount $ toUnsigned $ interpret e
    return $ r === fromIntegral r'
  where
    cmm = unlines
        [ "test ( bits64 buffer ) {"
        , "  bits64 ret;"
        , "  " ++ cmmType (knownWidth @w) ++ " x;"
        , "  x = " ++ exprToCmm e ++ ";"
        , "  (ret) = prim %popcnt" ++ show (widthBits (knownWidth @w)) ++ "(x);"
        , "  return (ret);"
        , "}"
        ]
