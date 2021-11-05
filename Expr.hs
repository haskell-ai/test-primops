-- | Expressions
module Expr where

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
    EAnd     :: Expr width -> Expr width -> Expr width
    EOr      :: Expr width -> Expr width -> Expr width
    ENot     :: Expr width -> Expr width
    EShl     :: Expr width -> Expr W8 -> Expr width
    EShr     :: Expr width -> Expr W8 -> Expr width
    ENegate  :: Expr width -> Expr width
    ENarrow  :: (KnownWidth wide)
             => Expr wide -> Expr narrow
    ESignExt :: (KnownWidth narrow)
             => Expr narrow -> Expr wide
    EZeroExt :: (KnownWidth narrow)
             => Expr narrow -> Expr wide
    ELit     :: Number width -> Expr width

l8 :: Integer -> Expr W8
l8 = ELit . n8

l16 :: Integer -> Expr W16
l16 = ELit . n16

l32 :: Integer -> Expr W32
l32 = ELit . n32

l64 :: Integer -> Expr W64
l64 = ELit . n64

instance KnownWidth width => Show (Expr width) where
    show = showExpr

showExpr :: forall width. (KnownWidth width)
         => Expr width -> String
showExpr e =
    case e of
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      ENot    a   -> parens $ "~" <> showExpr a
      EShl    a b -> binOp "<<" a b
      EShr    a b -> binOp ">>" a b
      ENegate a   -> parens $ "-" <> showExpr a
      ENarrow (a :: Expr wide) -> parens $ concat ["narrow<", show (knownWidth @wide), "> ", showExpr a]
      ESignExt (a :: Expr narrow) -> parens $ concat ["sext<", show (knownWidth @narrow), "> ", showExpr a]
      EZeroExt (a :: Expr narrow) -> parens $ concat ["zext<", show (knownWidth @narrow), "> ", showExpr a]
      ELit a      -> parens (show a <> "::" <> show (knownWidth @width))
  where
    binOp op a b = parens $ unwords [showExpr a, op, showExpr b]
    parens s = concat ["(", s, ")"]

-- * Generating arbitrary expressions

instance KnownWidth width => Arbitrary (Expr width) where
    arbitrary = genExpr
    shrink e =
        case e of
          EAdd     a b -> shrinkBinOp EAdd a b
          ESub     a b -> shrinkBinOp ESub a b
          EAnd     a b -> shrinkBinOp EAnd a b
          EOr      a b -> shrinkBinOp EOr  a b
          ENot     a   -> shrinkUnOp  ENot a
          EShl     a b -> shrinkBinOp EShl a b
          EShr     a b -> shrinkBinOp EShr a b
          ENegate  a   -> shrinkUnOp  ENegate a
          ENarrow  a   -> shrinkUnOp  ENarrow a
          ESignExt a   -> shrinkUnOp  ESignExt a
          EZeroExt a   -> shrinkUnOp  EZeroExt a
          ELit     a   -> map ELit (shrink a)
      where
        shrinkUnOp op a =
            [ op (ELit $ interpret a)
            , ELit $ interpret (op a)
            ] ++
            [ op a' | a' <- shrink a ]
        shrinkBinOp op a b =
            [ op (ELit $ interpret a) b
            , op a (ELit $ interpret b)
            , ELit $ interpret (op a b)
            ] ++
            [ op a' b' | (a', b') <- shrink (a, b) ]

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
            , binary EAnd
            , binary EOr
            , EShl <$> arbitrary <*> arbitrary
            , EShr <$> arbitrary <*> arbitrary
            , ENot <$> arbitrary
            , ENegate <$> arbitrary
            , do SomeExpr e <- arbitrary 
                 return $ ENarrow e
            --, do SomeExpr e <- arbitrary 
            --     return $ ESignExt e
            ]
            ++ if w == W8 then [] else
               [ EZeroExt <$> genExpr @W16
               , EZeroExt <$> genExpr @W32
               , EZeroExt <$> genExpr @W64
               ]

    w = knownWidth @width
    subexpr2 = scale (`div` 2) . genExpr'
    binary f = f <$> subexpr2 width <*> subexpr2 width

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
interpret (EAdd a b)   = liftBinOp (+)   (interpret a) (interpret b)
interpret (ESub a b)   = liftBinOp (-)   (interpret a) (interpret b)
interpret (EAnd a b)   = liftBinOp (.&.) (interpret a) (interpret b)
interpret (EOr  a b)   = liftBinOp (.|.) (interpret a) (interpret b)
interpret (EShl a b)   = interpret a `shiftL` fromIntegral (getNumber $ interpret b)
interpret (EShr a b)   = interpret a `shiftR` fromIntegral (getNumber $ interpret b)
interpret (ENot a)     = complement (interpret a)
interpret (ENegate a)  = negate (interpret a)
interpret (ENarrow a)  = truncateNumber (interpret a)
interpret (ESignExt a) = signExtNumber (interpret a)
interpret (EZeroExt a) = zeroExtNumber (interpret a)
interpret (ELit n)     = n
