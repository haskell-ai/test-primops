module CallishOp where

import Numeric.Natural
import Data.Bits
import Test.QuickCheck

import Width
import ToCmm
import Number
import Expr

popcnt :: forall w. (KnownWidth w) => UnaryCallish w WordSize
popcnt = UnaryCallish
    { name = "%popcnt" ++ show (widthBits (knownWidth @w))
    , refImpl = fromUnsigned . fromIntegral . popCount . toUnsigned
    }


prop_unary_callish_correct
    :: forall w. (KnownWidth w)
    => UnaryCallish w WordSize
    -> Expr w
    -> Property
prop_unary_callish_correct op e = ioProperty $ do
    r <- evalUnaryCallish op e
    return $ refImpl op (interpret e) === fromUnsigned r

data UnaryCallish w r
    = UnaryCallish { name :: String
                   , refImpl :: Number w -> Number r
                   }

evalUnaryCallish
    :: forall w. (KnownWidth w)
    => UnaryCallish w WordSize
    -> Expr w
    -> IO Natural
evalUnaryCallish op e =
    evalCmm ["-dcmm-lint"] cmm
  where
    cmm = unlines
        [ "test ( bits64 buffer ) {"
        , "  bits64 ret;"
        , "  " ++ cmmType (knownWidth @w) ++ " x;"
        , "  x = " ++ exprToCmm e ++ ";"
        , "  (ret) = prim " ++ name op ++ "(x);"
        , "  return (ret);"
        , "}"
        ]
