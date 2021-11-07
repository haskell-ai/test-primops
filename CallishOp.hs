module CallishOp where

import Numeric.Natural
import Data.Bits
import Data.Proxy
import Test.QuickCheck
import Data.Foldable (foldl')

import Width
import TestUtils
import ToCmm
import Number
import Expr

prop_callishs_correct :: Property
prop_callishs_correct = conjoin $
    [ property $ prop_callish_correct (pdep @w)
    | SomeWidth (_ :: Proxy w) <- allWidths
    ] ++
    [ property $ prop_callish_correct (pext @w)
    | SomeWidth (_ :: Proxy w) <- allWidths
    ]

popcnt :: forall w. (KnownWidth w) => Callish (Expr w) WordSize
popcnt = Callish
    { name = "%popcnt" ++ show (widthBits (knownWidth @w))
    , refImpl = fromUnsigned . fromIntegral . popCount . toUnsigned . interpret
    }

-- | Arguments are @(source, mask)@.
pdep :: forall w. (KnownWidth w) => Callish (Expr w, Expr w) WordSize
pdep = Callish
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
pext :: forall w. (KnownWidth w) => Callish (Expr w, Expr w) WordSize
pext = Callish
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
    => Callish args WordSize
    -> args
    -> Property
prop_callish_correct op e = ioProperty $ do
    r <- evalCallish op e
    return $ refImpl op e === r

data Callish args result
    = Callish { name :: String
              , refImpl :: args -> Number result
              }

class CmmArgs arg where
    argsToCmm :: arg -> String

instance (CmmArgs a, CmmArgs b) => CmmArgs (a,b) where
    argsToCmm (a,b) = concat ["(", argsToCmm a, ", ", argsToCmm b, ")"]

instance (KnownWidth w) => CmmArgs (Expr w) where
    argsToCmm a = exprToCmm a

evalCallish
    :: forall args. (CmmArgs args)
    => Callish args WordSize
    -> args
    -> IO (Number WordSize)
evalCallish op args =
    fromUnsigned <$> evalCmm gHC_PATH ["-dcmm-lint"] cmm
  where
    cmm = unlines
        [ "test ( bits64 buffer ) {"
        , "  bits64 ret;"
        , "  (ret) = prim " ++ name op ++ argsToCmm args ++ ";"
        , "  return (ret);"
        , "}"
        ]
