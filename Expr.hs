-- | Expressions
module Expr where

import Data.Foldable (foldl')
import Data.Type.Equality
import qualified Data.ByteString as BS
import Numeric.Natural
import Control.Monad
import Data.Bits as Bits
import Test.QuickCheck hiding ((.&.))
import Data.Proxy
import Prelude hiding (truncate)

import Width
import Number

data Expr (width :: Width) where
    EAdd     :: Expr width -> Expr width -> Expr width
    ESub     :: Expr width -> Expr width -> Expr width
    EMul     :: Expr width -> Expr width -> Expr width
    EDivU    :: Expr width -> Expr width -> Expr width
    ERemU    :: Expr width -> Expr width -> Expr width
    EDivS    :: Expr width -> Expr width -> Expr width
    ERemS    :: Expr width -> Expr width -> Expr width
    EAnd     :: Expr width -> Expr width -> Expr width
    EOr      :: Expr width -> Expr width -> Expr width
    EXOr     :: Expr width -> Expr width -> Expr width
    ENot     :: Expr width -> Expr width
    EShl     :: Expr width -> Expr WordSize -> Expr width
    EShrl    :: Expr width -> Expr WordSize -> Expr width
    EShra    :: Expr width -> Expr WordSize -> Expr width
    ENegate  :: Expr width -> Expr width
    ENarrow  :: (KnownWidth wide, wide `WiderThan` narrow)
             => Expr wide -> Expr narrow
    ESignExt :: (KnownWidth narrow, wide `WiderThan` narrow)
             => Expr narrow -> Expr wide
    EZeroExt :: (KnownWidth narrow, wide `WiderThan` narrow)
             => Expr narrow -> Expr wide
    ELoad    :: Expr W64 -> Expr width
    ELit     :: Number width -> Expr width

instance KnownWidth width => Num (Expr width) where
    (+) = EAdd
    (-) = ESub
    (*) = EMul
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

showExpr :: forall width. (KnownWidth width)
         => Expr width -> String
showExpr e =
    case e of
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EMul    a b -> binOp "*" a b
      EDivU   a b -> binOp "/u" a b
      EDivS   a b -> binOp "/s" a b
      ERemU   a b -> binOp "%u" a b
      ERemS   a b -> binOp "%s" a b
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      EXOr    a b -> binOp "^" a b
      ENot    a   -> parens $ "~" <> showExpr a
      EShl    a b -> binOp "<<" a b
      EShrl   a b -> binOp ">>l" a b
      EShra   a b -> binOp ">>a" a b
      ENegate a   -> parens $ "-" <> showExpr a
      ENarrow (a :: Expr wide) -> parens $ concat ["narrow<", show (knownWidth @wide), "->", show w, "> ", showExpr a]
      ESignExt (a :: Expr narrow) -> parens $ concat ["sext<", show (knownWidth @narrow), "->", show w, "> ", showExpr a]
      EZeroExt (a :: Expr narrow) -> parens $ concat ["zext<", show (knownWidth @narrow), "->", show w, "> ", showExpr a]
      ELoad off   -> parens (show (knownWidth @width) <> "* " <> showExpr off)
      ELit a      -> parens (show a <> "::" <> show (knownWidth @width))
  where
    w = knownWidth @width
    binOp op a b = parens $ unwords [showExpr a, op, showExpr b]
    parens s = concat ["(", s, ")"]

-- * Generating arbitrary expressions

instance KnownWidth width => Arbitrary (Expr width) where
    arbitrary = genExpr
    shrink e =
        case e of
          EAdd     a b -> shrinkBinOp EAdd  a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          ESub     a b -> shrinkBinOp ESub  a b ++ [ a | interpret b == 0 ]
          EMul     a b -> shrinkBinOp EMul  a b ++ [ a | interpret b == 1 ] ++ [ b | interpret a == 1 ]
          EDivU    a b -> shrinkDivOp EDivU a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          ERemU    a b -> shrinkDivOp ERemU a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          EDivS    a b -> shrinkDivOp EDivS a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          ERemS    a b -> shrinkDivOp ERemS a b ++ [ a | interpret b == 1 ] ++ [ 0 | interpret a == 0 ]
          EAnd     a b -> shrinkBinOp EAnd  a b ++ [ a | interpret b == ones ] ++ [ b | interpret a == ones ]
          EOr      a b -> shrinkBinOp EOr   a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          EXOr     a b -> shrinkBinOp EXOr  a b ++ [ a | interpret b == 0 ] ++ [ b | interpret a == 0 ]
          ENot     a   -> shrinkUnOp  ENot  a   ++ [a]
          EShl     a b -> shrinkBinOp EShl  a b ++ [ a | interpret b == 0 ]
          EShrl    a b -> shrinkBinOp EShrl a b ++ [ a | interpret b == 0 ]
          EShra    a b -> shrinkBinOp EShra a b ++ [ a | interpret b == 0 ]
          ENegate  a   -> shrinkUnOp  ENegate a ++ [a]
          ENarrow  a   -> shrinkUnOp  ENarrow a  ++ [ ENarrow b | ENarrow b <- pure a, Just WiderThanProof <- pure $ b `isWiderThan` e ] ++ [ b | EZeroExt b <- pure a, Just Refl <- pure $ e `isSameWidth` b ]
          ESignExt a   -> shrinkUnOp  ESignExt a ++ [ ESignExt b | ESignExt b <- pure a, Just WiderThanProof <- pure $ e `isWiderThan` b ] ++ [ EZeroExt a ]
          EZeroExt a   -> shrinkUnOp  EZeroExt a ++ [ EZeroExt b | EZeroExt b <- pure a, Just WiderThanProof <- pure $ e `isWiderThan` b ]
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
genExpr' width = sized gen
  where
    gen 0 = ELit <$> arbitrary
    gen _ = do
        oneof $
            [ ELit <$> arbitrary
            , binary EAdd
            , binary ESub
            , binary EMul
            , divOp  EDivU
            , divOp  ERemU
            , divOp  EDivS
            , divOp  ERemS
            , binary EAnd
            , binary EOr
            , binary EXOr
            -- N.B. C--'s shift primops are undefined with shifts outside of
            -- [0,WORD_SIZE).
            , EShl <$> arbitrary <*> arbitraryShift
            , EShrl <$> arbitrary <*> arbitraryShift
            -- See https://gitlab.haskell.org/ghc/ghc/-/issues/20626
            -- , EShra <$> arbitrary <*> arbitraryShift
            , ENot <$> arbitrary
            , ENegate <$> arbitrary
            , do off <- chooseNumber (0, fromIntegral bufferSize-1)
                 return $ ELoad $ ELit off
            ]
            ++ narrowings @width (\(_ :: Proxy narrow) -> EZeroExt <$> genExpr @narrow)
            ++ narrowings @width (\(_ :: Proxy narrow) -> ESignExt <$> genExpr @narrow)
            ++ extensions @width (\(_ :: Proxy wide)   -> ENarrow  <$> genExpr @wide)

    arbitraryShift = ELit <$> chooseNumber (0, 64-1)
    subexpr2 = scale (`div` 2) . genExpr'
    binary f = f <$> subexpr2 width <*> subexpr2 width
    divOp f = f <$> subexpr2 width <*> nonzero (subexpr2 width)
    nonzero = flip suchThat $ \x -> interpret x /= 0

-- * SomeExpr

data SomeExpr where
    SomeExpr :: KnownWidth width => Expr width -> SomeExpr

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

interpret :: forall width. (KnownWidth width)
          => Expr width -> Number width
interpret (EAdd a b)   = interpret a + interpret b
interpret (ESub a b)   = interpret a - interpret b
interpret (EMul a b)   = interpret a * interpret b
interpret (EDivS a b)  = interpret a `divS` interpret b
interpret (ERemS a b)  = interpret a `remS` interpret b
interpret (EDivU a b)  = interpret a `divU` interpret b
interpret (ERemU a b)  = interpret a `remU` interpret b
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
buffer = BS.pack $ take (fromIntegral bufferSize) [ fromIntegral i | i <- [0..] ]

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
