-- | Test the @MulMayOflo@ machop.

module MulMayOverflow (prop_mul_may_oflo_correct) where

import Data.Proxy
import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Width
import RunGhc
import ToCmm
import Number
import Expr
import GHC.Natural

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
    (r,cmm) <- evalMulMayOflo em x y
    let (does_oflo) = case r of
            Right eval_ov -> Just (eval_ov /= 0)
            Left{} -> Nothing

    return
       $ classify (isJust does_oflo && fromJust does_oflo && not should_overflow) "false-overflow"
       $ counterexample
            ("should_overflow:" ++ show should_overflow ++
            " prod:" ++ show prod ++
            " cmm_result:" ++ show r ++
            " arguments:" ++ show [x, y] ++
            " cmm:" ++ cmm)
       $ ((should_overflow ==> does_oflo) .&&.
          property (isJust does_oflo))
  where
    (min_bound, max_bound) = signedBounds (knownWidth @w)
    prod = toSigned (interpret x) * toSigned (interpret y)
    should_overflow = prod < min_bound || prod > max_bound

evalMulMayOflo
    :: forall w. (KnownWidth w)
    => EvalMethod
    -> Expr w
    -> Expr w
    -> IO (Either ProcessFailure (Number w),String)
evalMulMayOflo em x y = do
    eval_res <- evalCmm em w cmm :: IO (Either ProcessFailure Natural)
    return (fmap fromUnsigned eval_res,cmm)
  where
    cmm = unlines
        [ "test ( " <> cmmWordType <> " buffer ) {"
        , "  " <> cmmType w <> " ret, x, y;"
        , "  x = " ++ exprToCmm x ++ ";"
        , "  y = " ++ exprToCmm y ++ ";"
        , "  ret = %mulmayoflo(x,y);"
        , "  return (ret);"
        , "}"
        ]
    w = knownWidth @w
