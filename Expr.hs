-- | Expressions
module Expr where

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
    ENegate  :: Expr width -> Expr width
    ENarrow  :: (KnownWidth wide, Wider wide narrow)
             => Expr wide -> Expr narrow
    ESignExt :: (KnownWidth narrow, Wider wide narrow)
             => Expr narrow -> Expr wide
    EZeroExt :: (KnownWidth narrow, Wider wide narrow)
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

instance Show (Expr width) where
    show = showExpr

showExpr :: Expr width -> String
showExpr e =
    case e of
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EAnd    a b -> binOp "-" a b
      EOr     a b -> binOp "-" a b
      ENegate a   -> parens $ "-" <> showExpr a
      ENarrow (a :: Expr wide) -> parens $ concat ["narrow<", show (knownWidth @wide), "> ", showExpr a]
      ESignExt (a :: Expr narrow) -> parens $ concat ["sext<", show (knownWidth @narrow), "> ", showExpr a]
      EZeroExt (a :: Expr narrow) -> parens $ concat ["zext<", show (knownWidth @narrow), "> ", showExpr a]
      ELit a      -> show a
  where
    binOp op a b = parens $ unwords [showExpr a, op, showExpr b]
    parens s = concat ["(", s, ")"]

-- * Generating arbitrary expressions

genExpr :: forall width. (KnownWidth width)
        => Proxy width -> Gen (Expr width)
genExpr width = sized gen
  where
    gen 0 = ELit <$> arbitrary
    gen _ = do
        oneof
            [ ELit <$> arbitrary
            , binary EAdd
            , binary ESub
            , binary EAnd
            , binary EOr
            ]

    subexpr2 = scale (`div` 2) . genExpr
    binary f = f <$> subexpr2 width <*> subexpr2 width

-- * Interpreter

interpret :: forall width. (KnownWidth width)
          => Expr width -> Number width
interpret (EAdd a b)   = liftBinOp (+)   (interpret a) (interpret b)
interpret (ESub a b)   = liftBinOp (-)   (interpret a) (interpret b)
interpret (EAnd a b)   = liftBinOp (.&.) (interpret a) (interpret b)
interpret (EOr  a b)   = liftBinOp (.|.) (interpret a) (interpret b)
interpret (ENegate a)  = negateNumber (interpret a)
interpret (ENarrow a)  = truncateNumber (interpret a)
interpret (ESignExt a) = signExtNumber (interpret a)
interpret (EZeroExt a) = zeroExtNumber (interpret a)
interpret (ELit n)     = n
