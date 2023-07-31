-- | Test the @MulMayOflo@ machop.

module MulMayOverflow (prop_mul_may_oflo_correct) where

import Data.Proxy
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Width
import RunGhc
import ToCmm
import Number
import Expr

-- | @MO_MulMayOflo@ tests whether a signed product may overflow the target
-- width. It:
--
--  - Must return nonzero if the result overflows
--  - May return zero otherwise
--
-- We cannot test this like other MachOps since its result is not well-defined.
prop_mul_may_oflo_correct :: EvalMethod -> TestTree
prop_mul_may_oflo_correct em = testGroup "MulMayOflo"
    [ testProperty (show (knownWidth @w)) (prop @w em Proxy)
    | SomeWidth (_ :: Proxy w) <- allWidths
    ]

prop :: forall w. (KnownWidth w)
     => EvalMethod
     -> Proxy w
     -> Expr w -> Expr w
     -> Property
prop em Proxy x y = ioProperty $ do
    r <- evalMulMayOflo em x y
    let does_oflo = r /= 0
    return
       $ classify (does_oflo && not does_overflow) "false-overflow"
       $ counterexample (show (does_overflow, prod))
       $ (does_overflow ==> does_oflo)
  where
    (min_bound, max_bound) = signedBounds (knownWidth @w)
    prod = toSigned (interpret x) * toSigned (interpret y)
    does_overflow = prod < min_bound || prod > max_bound

evalMulMayOflo
    :: forall w. (KnownWidth w)
    => EvalMethod
    -> Expr w
    -> Expr w
    -> IO (Number WordSize)
evalMulMayOflo em x y =
    fromUnsigned <$> evalCmm em wordSize cmm
  where
    cmm = unlines
        [ "test ( " <> cmmWordType <> " buffer ) {"
        , "  " <> cmmType w <> " ret, x, y;"
        , "  x = " ++ exprToCmm x ++ ";"
        , "  y = " ++ exprToCmm y ++ ";"
        , "  ret = %mulmayoflo(x,y);"
        , "  return ("++widenOp++"(ret));"
        , "}"
        ]
    widenOp = "%zx" ++ show (widthBits wordSize)
    w = knownWidth @w
