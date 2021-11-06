-- | Expressions
module Expr
    ( -- * Relational operators
      RelationalOp(..)
    , allRelationalOps
    , relationalOpString
      -- * Expressoins
    , Expr(..), SomeExpr(..)
      -- ** Convenient helpers
    , l8, l16, l32, l64
      -- * Showing
    , showExpr
    , showInterpretedExpr
    , exprToTree
      -- ** Generating
    , genExpr
      -- ** Interpreting
    , buffer, load
    , interpret
    ) where

import Data.Foldable (foldl')
import Data.Type.Equality
import qualified Data.ByteString as BS
import Numeric.Natural
import Control.Monad
import Data.Bits as Bits
import Data.List (intercalate)
import Test.QuickCheck hiding ((.&.))
import Data.Tree
import Data.Proxy
import Prelude hiding (truncate)

import Width
import Number

data RelationalOp
    = REq
    | RNEq
    | RGT Signedness
    | RGE Signedness
    | RLT Signedness
    | RLE Signedness
    deriving (Eq, Ord, Show, Read)

allRelationalOps :: [RelationalOp]
allRelationalOps = concat
    [ [ REq, RNEq]
    , signed RGT
    , signed RGE
    , signed RLT
    , signed RLE
    ]
  where
    signed f = [f Signed, f Unsigned]

relationalOpString :: RelationalOp -> String
relationalOpString op =
    case op of
      REq    -> "=="
      RNEq   -> "!="
      RGT  s -> ">"  ++ signednessTag s
      RGE  s -> ">=" ++ signednessTag s
      RLT  s -> "<"  ++ signednessTag s
      RLE  s -> "<=" ++ signednessTag s

instance Arbitrary RelationalOp where
    arbitrary = oneof
        [ pure REq
        , pure RNEq
        , RGT <$> arbitrary
        , RGE <$> arbitrary
        , RLT <$> arbitrary
        , RLE <$> arbitrary
        ]

data Expr (width :: Width) where
    ERel     :: (KnownWidth width) => RelationalOp -> Expr width -> Expr width -> Expr WordSize

    EAdd     :: Expr width -> Expr width -> Expr width
    ESub     :: Expr width -> Expr width -> Expr width
    EMul     :: Expr width -> Expr width -> Expr width
    EDiv     :: Signedness -> Expr width -> Expr width -> Expr width
    ERem     :: Signedness -> Expr width -> Expr width -> Expr width

    EAnd     :: Expr width -> Expr width -> Expr width
    EOr      :: Expr width -> Expr width -> Expr width
    EXOr     :: Expr width -> Expr width -> Expr width
    ENot     :: Expr width -> Expr width
    EShl     :: Expr width -> Expr WordSize -> Expr width
    EShrl    :: Expr width -> Expr WordSize -> Expr width
    EShra    :: Expr width -> Expr WordSize -> Expr width

    ENegate  :: Expr width -> Expr width

    ENarrow  :: forall wide narrow. (KnownWidth wide, wide `WiderThan` narrow)
             => Expr wide -> Expr narrow
    ESignExt :: forall narrow wide. (KnownWidth narrow, wide `WiderThan` narrow)
             => Expr narrow -> Expr wide
    EZeroExt :: forall narrow wide. (KnownWidth narrow, wide `WiderThan` narrow)
             => Expr narrow -> Expr wide

    ELoad    :: Expr W64 -> Expr width
    ELit     :: Number width -> Expr width

instance KnownWidth width => Num (Expr width) where
    (+) = EAdd
    (-) = ESub
    (*) = EMul
    signum = error "Expr(signum)"
    negate = ENegate
    abs = id
    fromInteger = ELit . fromInteger

l8 :: Natural -> Expr W8
l8 = ELit . n8

l16 :: Natural -> Expr W16
l16 = ELit . n16

l32 :: Natural -> Expr W32
l32 = ELit . n32

l64 :: Natural -> Expr W64
l64 = ELit . n64

instance KnownWidth width => Show (Expr width) where
    show = showExpr

-- * Generating arbitrary expressions

instance KnownWidth width => Arbitrary (Expr width) where
    arbitrary = genExpr
    shrink e =
        case e of
          ERel  op a b -> shrinkBinOp (ERel op) a b

          EAdd     a b -> shrinkBinOp EAdd  a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          ESub     a b -> shrinkBinOp ESub  a b ++ [ a | interpret b == 0 ]
          EMul     a b -> shrinkBinOp EMul  a b ++ [ a | interpret b == 1 ] ++ [ b | interpret a == 1 ]
          EDiv   s a b -> shrinkDivOp (EDiv s) a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          ERem   s a b -> shrinkDivOp (ERem s) a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          EAnd     a b -> shrinkBinOp EAnd  a b ++ [ a | interpret b == ones ] ++ [ b | interpret a == ones ]
          EOr      a b -> shrinkBinOp EOr   a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          EXOr     a b -> shrinkBinOp EXOr  a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          ENot     a   -> shrinkUnOp  ENot  a   ++ [a]
          EShl     a b -> shrinkBinOp EShl  a b ++ [ a | interpret b == 0 ]
          EShrl    a b -> shrinkBinOp EShrl a b ++ [ a | interpret b == 0 ]
          EShra    a b -> shrinkBinOp EShra a b ++ [ a | interpret b == 0 ]
          ENegate  a   -> shrinkUnOp  ENegate a ++ [a]
          ENarrow  a   -> shrinkUnOp  ENarrow a
                          ++ [ ENarrow b | ENarrow b <- pure a, Just WiderThanProof <- pure $ b `isWiderThan` e ]
                          ++ [ b | EZeroExt b <- pure a, Just Refl <- pure $ e `isSameWidth` b ]
          ESignExt a   -> shrinkUnOp  ESignExt a
                          ++ [ ESignExt b | ESignExt b <- pure a, Just WiderThanProof <- pure $ e `isWiderThan` b ]
                          ++ [ EZeroExt a ]
          EZeroExt a   -> shrinkUnOp  EZeroExt a
                          ++ [ EZeroExt b | EZeroExt b <- pure a, Just WiderThanProof <- pure $ e `isWiderThan` b ]
          ELoad    a   -> shrinkUnOp  ELoad a
          ELit     a   -> map ELit (shrink a)
      where
        shrinkUnOp op a =
            [ ELit $ interpret (op a) ] ++
            [ op a' | a' <- shrink a ]
        shrinkBinOp op a b =
            [ ELit $ interpret (op a b) ] ++
            [ op a' b' | (a', b') <- shrink (a, b) ]
        shrinkDivOp op a b =
            [ ELit $ interpret (op a b) ] ++
            [ op a' b'
            | (a', b') <- shrink (a, b)
            , interpret b' /= 0
            ]

genExpr :: forall width. (KnownWidth width) 
        => Gen (Expr width)
genExpr = genExpr' (Proxy @width)

genExpr' :: forall width. (KnownWidth width) 
         => Proxy width -> Gen (Expr width)
genExpr' _width = sized gen
  where
    gen :: Int -> Gen (Expr width)
    gen 0 = litGen
    gen _ = do
        oneof $
            [ litGen
            , loadGen
            ]
            ++ arithmeticGens
            ++ bitwiseGens
            ++ shiftGens
            ++ narrowings @width (\(_ :: Proxy narrow) -> EZeroExt <$> genExpr @narrow)
            ++ narrowings @width (\(_ :: Proxy narrow) -> ESignExt <$> genExpr @narrow)
            ++ extensions @width (\(_ :: Proxy wide)   -> ENarrow  <$> genExpr @wide)
            ++ relationalGens

    litGen :: Gen (Expr width)
    litGen = ELit <$> arbitrary

    loadGen :: Gen (Expr width)
    loadGen = do
        off <- chooseNumber (0, fromIntegral bufferSize-1)
        return $ ELoad $ ELit off

    arithmeticGens :: [Gen (Expr width)]
    arithmeticGens =
        [ binOp EAdd
        , binOp ESub
        , binOp EMul
        , divOp EDiv
        , divOp ERem
        , ENegate <$> arbitrary
        ]

    bitwiseGens :: [Gen (Expr width)]
    bitwiseGens =
        [ ENot <$> arbitrary
        , binOp EAnd
        , binOp EOr
        , binOp EXOr
        ]

    shiftGens :: [Gen (Expr width)]
    shiftGens =
        [ -- N.B. C--'s shift primops are undefined with shifts outside of
          -- [0,WORD_SIZE).
          EShl <$> arbitrary <*> arbitraryShift
        , EShrl <$> arbitrary <*> arbitraryShift
          -- See https://gitlab.haskell.org/ghc/ghc/-/issues/20626
        --, EShra <$> arbitrary <*> arbitraryShift
        ]

    relationalGens :: [Gen (Expr width)]
    relationalGens
      | Just Refl <- Proxy @width `isSameWidth` Proxy @WordSize
      = [ do op <- arbitrary
             binOp (ERel op)
        ]
      | otherwise = []

    arbitraryShift = ELit <$> chooseNumber (0, 64-1)
    subexpr2 = scale (`div` 2) genExpr
    binOp f = f <$> subexpr2 <*> subexpr2
    divOp f = f <$> arbitrary <*> subexpr2 <*> nonzero subexpr2
    nonzero = flip suchThat $ \x -> interpret x /= 0

-- * SomeExpr

data SomeExpr where
    SomeExpr :: (KnownWidth width) => Expr width -> SomeExpr

instance Show SomeExpr where
    show (SomeExpr (e :: Expr width)) =
        "SomeExpr @" <> show (knownWidth @width) <> " " <> show e

instance Arbitrary SomeExpr where
    arbitrary = withArbitraryWidth arbitrary

withArbitraryWidth
    :: (forall width. KnownWidth width => Gen (Expr width))
    -> Gen SomeExpr
withArbitraryWidth f =
    oneof [ SomeExpr <$> f @W8
          , SomeExpr <$> f @W16
          , SomeExpr <$> f @W32
          , SomeExpr <$> f @W64
          ]

-- * Interpreter

boolVal :: Bool -> Number WordSize
boolVal True  = 1
boolVal False = 0

interpretRelOp
    :: forall width. (KnownWidth width)
    => RelationalOp
    -> Number width
    -> Number width
    -> Bool
interpretRelOp op =
    case op of
      REq   -> (==)
      RNEq  -> (/=)
      RGT s -> signed s (>)
      RGE s -> signed s (>=)
      RLT s -> signed s (<)
      RLE s -> signed s (<=)
  where
    signed :: Signedness
           -> (forall a. (Ord a) => a -> a -> Bool)
           -> Number width
           -> Number width
           -> Bool
    signed Unsigned f x y = f x y
    signed Signed   f x y = f (toSigned x) (toSigned y)

interpret :: forall width. (KnownWidth width)
          => Expr width -> Number width
interpret (ERel o a b) = boolVal $ interpretRelOp o (interpret a) (interpret b)
interpret (EAdd a b)   = interpret a + interpret b
interpret (ESub a b)   = interpret a - interpret b
interpret (EMul a b)   = interpret a * interpret b
interpret (EDiv s a b) = divNumber s (interpret a) (interpret b)
interpret (ERem s a b) = remNumber s (interpret a) (interpret b)
interpret (EAnd a b)   = interpret a .&. interpret b
interpret (EOr  a b)   = interpret a .|. interpret b
interpret (EXOr a b)   = interpret a `xor` interpret b
interpret (EShl a b)   = interpret a `shiftL` fromIntegral (toUnsigned $ interpret b)
interpret (EShrl a b)  = interpret a `shiftRl` fromIntegral (toUnsigned $ interpret b)
interpret (EShra a b)  = interpret a `shiftRa` fromIntegral (toUnsigned $ interpret b)
interpret (ENot a)     = complement (interpret a)
interpret (ENegate a)  = negate (interpret a)
interpret (ENarrow a)  = truncateNumber (interpret a)
interpret (ESignExt a) = signExtNumber (interpret a)
interpret (EZeroExt a) = zeroExtNumber (interpret a)
interpret (ELoad off)  = load $ toUnsigned $ interpret off
interpret (ELit n)     = n

bufferSize :: Natural
bufferSize = 1 `shiftL` 22

buffer :: BS.ByteString
buffer = BS.pack $ take (fromIntegral bufferSize) [ fromIntegral i | i <- [(0 :: Int) ..] ]

validOffset :: Natural -> Bool
validOffset off = off < bufferSize

data Endianness = LittleEndian | BigEndian

endianness :: Endianness
endianness = LittleEndian

load :: forall width. (KnownWidth width)
     => Natural -> Number width
load off
  | not (validOffset off) = error $ "invalid offset " <> show off
  | otherwise             =
      let xs = BS.unpack $ BS.take (w `div` 8) $ BS.drop (fromIntegral off) buffer
          w = widthBits (knownWidth @width)
          swap = case endianness of
                   LittleEndian -> id
                   BigEndian    -> reverse
      in foldl' (.|.) 0
         [ fromIntegral n `shiftL` (8*i)
         | (i,n) <- zip [0..] (swap xs)
         ]

exprToTree
    :: forall width a. (KnownWidth width)
    => (forall w. (KnownWidth w) => Expr w -> a)
    -> Expr width
    -> Tree (String, a)
exprToTree f e =
    case e of
      ERel op a b -> binOp (relationalOpString op) a b
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EMul    a b -> binOp "*" a b
      EDiv  s a b -> binOp ("/"++signednessTag s) a b
      ERem  s a b -> binOp ("%"++signednessTag s) a b
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      EXOr    a b -> binOp "^" a b
      ENot    a   -> unOp  "~" a
      EShl    a b -> binOp "<<" a b
      EShrl   a b -> binOp ">>l" a b
      EShra   a b -> binOp ">>a" a b
      ENegate a   -> unOp "-" a
      ENarrow (a :: Expr wide)    -> unOp (concat ["narrow<", show (knownWidth @wide), "->", show w, ">"]) a
      ESignExt (a :: Expr narrow) -> unOp (concat ["sext<", show (knownWidth @narrow), "->", show w, ">"]) a
      EZeroExt (a :: Expr narrow) -> unOp (concat ["zext<", show (knownWidth @narrow), "->", show w, ">"]) a
      ELoad off   -> unOp (concat ["load<", show (knownWidth @width), ">"]) off
      ELit a      -> leaf (show a <> "::" <> show (knownWidth @width))
  where
    w = knownWidth @width
    binOp :: forall w1 w2. (KnownWidth w1, KnownWidth w2)
          => String -> Expr w1 -> Expr w2 -> Tree (String, a)
    binOp op a b = Node (op, f e) [exprToTree f a, exprToTree f b]
    unOp :: forall w1. (KnownWidth w1)
         => String -> Expr w1 -> Tree (String, a)
    unOp op a = Node (op, f e) [exprToTree f a]
    leaf s = Node (s, f e) []

showParenTree :: Tree String -> String
showParenTree (Node lbl [a, b]) =
    unwords [showWithParens a, lbl, showWithParens b]
  where
    showWithParens x
      | isLeaf x  = showParenTree x
      | otherwise = parens $ showParenTree x
    isLeaf (Node _ []) = True
    isLeaf _ = False
showParenTree (Node lbl []) = lbl
showParenTree (Node lbl xs) =
    lbl <> parens (intercalate ", " $ map showParenTree xs)

parens :: String -> String
parens s = concat ["(", s, ")"]

showExpr :: forall width. (KnownWidth width)
         => Expr width -> String
showExpr = showParenTree . fmap fst . exprToTree (const ())

showInterpretedExpr
    :: forall width. (KnownWidth width)
    => Expr width -> Tree String
showInterpretedExpr =
    fmap (\(a,b) -> a ++ "\t\t" ++ show b) . exprToTree (toUnsigned . interpret)
