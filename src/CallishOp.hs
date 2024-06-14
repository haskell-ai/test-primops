-- | Correctness tests for callish Cmm MachOps.
module CallishOp
    ( prop_callish_ops_correct
      -- * Individual tests
    , popcnt, pdep, pext
    , refImpl
    , prop_callish_correct
      -- * Evaluating callish machops
    , evalCallishOp
    , evalCallishOpCmm
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

prop_callish_ops_correct :: EvalMethod -> TestTree
prop_callish_ops_correct em = testGroup "callish ops"
    [ testCallishOpNoW8 "bswap"  (\(_ :: Proxy w) -> toProp $ bswap @w)
    , testCallishOp "popcnt" (\(_ :: Proxy w) -> toProp $ popcnt @w)
    , testCallishOp "pdep"   (\(_ :: Proxy w) -> toProp $ pdep @w)
    , testCallishOp "pext"   (\(_ :: Proxy w) -> toProp $ pext @w)
    ]
  where
    toProp :: forall args w. (CmmArgs args, Arbitrary args, Show args, KnownWidth w)
           => CallishOp args w
           -> Property
    toProp op = property $ prop_callish_correct em op

    testCallishOp, testCallishOpNoW8
        :: String
        -> (forall w. (KnownWidth w) => Proxy w -> Property)
        -> TestTree
    testCallishOp nm f = testGroup nm
        [ testProperty (show (knownWidth @w)) (f @w Proxy)
        | SomeWidth (_ :: Proxy w) <- allWidths
        ]
    testCallishOpNoW8 nm f = testGroup nm
        [ testProperty (show (knownWidth @w)) (f @w Proxy)
        | SomeWidth (_ :: Proxy w) <-
            [SomeWidth (Proxy @W16),
             SomeWidth (Proxy @W32),
             SomeWidth (Proxy @W64)]
        ]

popcnt :: forall w. (KnownWidth w) => CallishOp (Expr w) WordSize
popcnt = CallishOp
    { name = "%popcnt" ++ show (widthBits (knownWidth @w))
    , refImpl = fromUnsigned . fromIntegral . popCount . toUnsigned . interpret
    }

-- | Arguments are @(source, mask)@.
pdep :: forall w. (KnownWidth w) => CallishOp (Expr w, Expr w) w
pdep = CallishOp
    { name = "%pdep" ++ show (widthBits (knownWidth @w))
    , refImpl = uncurry ref
    }
  where
    ref :: Expr w -> Expr w -> Number w
    ref x0 mask0 = fromUnsigned $ fromBits $ go (exprBits mask0) (exprBits x0)
      where
        exprBits = toBits . interpret

        go (True:mask)  (b:rest) = b     : go mask rest
        go (False:mask) rest     = False : go mask rest
        go []           _        = []
        go _            []       = error "pdep: ran out of bits"

-- | Arguments are @(source, mask)@.
pext :: forall w. (KnownWidth w) => CallishOp (Expr w, Expr w) w
pext = CallishOp
    { name = "%pext" ++ show (widthBits (knownWidth @w))
    , refImpl = uncurry ref
    }
  where
    ref :: Expr w -> Expr w -> Number w
    ref x mask =
        fromUnsigned
        $ fromBits
        [ b | (True, b) <- zip (exprBits mask) (exprBits x) ]
      where
        exprBits = toBits . interpret

-- | Arguments are @(source, mask)@.
bswap :: forall w. (KnownWidth w) => CallishOp (Expr w) w
bswap = CallishOp
    { name = "%bswap" ++ show (widthBits (knownWidth @w))
    , refImpl = ref . interpret
    }
  where
    ref :: Number w -> Number w
    ref = fromBytes . reverse . toBytes
      where
        toBytes = chunk 8 . toBits
        fromBytes = fromUnsigned . fromBits . concat

toBits :: forall w. (KnownWidth w) => Number w -> [Bool]
toBits x =
    [ x `testBit` i | i <- [0..widthBits (knownWidth @w)-1] ]

fromBits :: [Bool] -> Natural
fromBits bits = foldl' (.|.) 0 [ bit i | (i, True) <- zip [0..] bits ]

prop_callish_correct
    :: forall args width. (CmmArgs args, KnownWidth width)
    => EvalMethod
    -> CallishOp args width
    -> args
    -> Property
prop_callish_correct em op args = counterexample (evalCallishOpCmm op args) $ ioProperty $ do
    r <- evalCallishOp em op args
    return $ refImpl op args === r

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
    :: forall args width. (CmmArgs args, KnownWidth width)
    => EvalMethod
    -> CallishOp args width
    -> args
    -> IO (Number width)
evalCallishOp em op args =
    fromUnsigned <$> throwFailure (evalCmm em (knownWidth @width) (evalCallishOpCmm op args))

evalCallishOpCmm
    :: forall args width. (CmmArgs args, KnownWidth width)
    => CallishOp args width
    -> args
    -> String
evalCallishOpCmm op args = unlines
    [ "test ( " <> cmmWordType <> " buffer ) {"
    , "  " <> cmmType width <> " ret;"
    , "  (ret) = prim " ++ name op ++ argList ++ ";"
    , "  return (ret);"
    , "}"
    ]
  where
    argList = parens $ commaList [exprToCmm e | SomeExpr e <- getArgs args]
    width = knownWidth @width

-- General Utils ---------------------------------------------------------------

-- 'chunk' a list, from sbv
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

