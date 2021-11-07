-- | Correctness tests for callish Cmm MachOps.
module CallishOp
    ( prop_callish_ops_correct
      -- * Individual tests
    , popcnt, pdep, pext
    , prop_callish_correct
      -- * Evaluating callish machops
    , evalCallishOp
    ) where

import Numeric.Natural
import Data.Bits
import Data.Proxy
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Foldable (foldl')

import Width
import RunGhc
import ToCmm
import Number
import Expr

prop_callish_ops_correct :: Compiler -> TestTree
prop_callish_ops_correct comp = testGroup "callish ops"
    [ testCallishOp "popcnt" (\(_ :: Proxy w) -> toProp $ popcnt @w)
    , testCallishOp "pdep"   (\(_ :: Proxy w) -> toProp $ pdep @w)
    , testCallishOp "pext"   (\(_ :: Proxy w) -> toProp $ pext @w)
    ]
  where
    toProp :: forall args. (CmmArgs args, Arbitrary args, Show args)
           => CallishOp args WordSize
           -> Property
    toProp op = property $ prop_callish_correct comp op

    testCallishOp
        :: String
        -> (forall w. (KnownWidth w) => Proxy w -> Property)
        -> TestTree
    testCallishOp nm f = testGroup nm
        [ testProperty (show (knownWidth @w)) (f @w Proxy)
        | SomeWidth (_ :: Proxy w) <- allWidths
        ]

popcnt :: forall w. (KnownWidth w) => CallishOp (Expr w) WordSize
popcnt = CallishOp
    { name = "%popcnt" ++ show (widthBits (knownWidth @w))
    , refImpl = fromUnsigned . fromIntegral . popCount . toUnsigned . interpret
    }

-- | Arguments are @(source, mask)@.
pdep :: forall w. (KnownWidth w) => CallishOp (Expr w, Expr w) WordSize
pdep = CallishOp
    { name = "%pdep" ++ show (widthBits (knownWidth @w))
    , refImpl = uncurry ref
    }
  where
    ref :: Expr w -> Expr w -> Number WordSize
    ref x0 mask0 = fromUnsigned $ fromBits $ go (exprBits mask0) (exprBits x0)
      where
        exprBits = toBits . interpret

        go (True:mask)  (b:rest) = b     : go mask rest
        go (False:mask) rest     = False : go mask rest
        go []           _        = []
        go _            []       = error "pdep: ran out of bits"

-- | Arguments are @(source, mask)@.
pext :: forall w. (KnownWidth w) => CallishOp (Expr w, Expr w) WordSize
pext = CallishOp
    { name = "%pext" ++ show (widthBits (knownWidth @w))
    , refImpl = uncurry ref
    }
  where
    ref :: Expr w -> Expr w -> Number WordSize
    ref x mask =
        fromUnsigned
        $ fromBits
        [ b | (True, b) <- zip (exprBits mask) (exprBits x) ]
      where
        exprBits = toBits . interpret

toBits :: forall w. (KnownWidth w) => Number w -> [Bool]
toBits x =
    [ x `testBit` i | i <- [0..widthBits (knownWidth @w)-1] ]

fromBits :: [Bool] -> Natural
fromBits bits = foldl' (.|.) 0 [ bit i | (i, True) <- zip [0..] bits ]

prop_callish_correct
    :: forall args. (CmmArgs args)
    => Compiler
    -> CallishOp args WordSize
    -> args
    -> Property
prop_callish_correct comp op e = ioProperty $ do
    r <- evalCallishOp comp op e
    return $ refImpl op e === r

data CallishOp args result
    = CallishOp { name :: String
              , refImpl :: args -> Number result
              }

class CmmArgs arg where
    getArgs :: arg -> [SomeExpr]

instance (CmmArgs a, CmmArgs b) => CmmArgs (a,b) where
    getArgs (a,b) = getArgs a ++ getArgs b

instance (KnownWidth w) => CmmArgs (Expr w) where
    getArgs a = [SomeExpr a]

evalCallishOp
    :: forall args. (CmmArgs args)
    => Compiler
    -> CallishOp args WordSize
    -> args
    -> IO (Number WordSize)
evalCallishOp comp op args =
    fromUnsigned <$> evalCmm comp (evalCallishOpCmm op args)

evalCallishOpCmm
    :: forall args. (CmmArgs args)
    => CallishOp args WordSize
    -> args
    -> String
evalCallishOpCmm op args = unlines
    [ "test ( bits64 buffer ) {"
    , "  bits64 ret;"
    , "  (ret) = prim " ++ name op ++ argList ++ ";"
    , "  return (ret);"
    , "}"
    ]
  where
    argList = parens $ commaList [exprToCmm e | SomeExpr e <- getArgs args]
