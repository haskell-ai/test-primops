module Expr.Parse
    ( parseExpr
    , prop_roundtrips
    ) where

import Control.Monad
import Control.Monad.Trans.Except
import Data.Type.Equality
import Data.Proxy
import Control.Applicative
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe
import Test.QuickCheck

import Number
import Width
import Expr

type Parser = Parsec Void String

parseExpr :: forall width. (KnownWidth width) => String -> Expr width
parseExpr s = fromMaybe (error "parseExpr: incorrect width") $ do
    SomeExpr (e :: Expr w) <- pure $ parseSomeExpr s
    case Proxy @w `compareWidths` Proxy @width of
        SameWidth -> Just e
        _ -> Nothing

parseSomeExpr :: String -> SomeExpr
parseSomeExpr s =
    case runParser expr "input" s of
      Left err -> error $ errorBundlePretty err
      Right e  -> e

expr :: Parser SomeExpr
expr = makeExprParser term [operators] <?> "expression"

term :: Parser SomeExpr
term = parens expr <|> lit

operators :: [Operator Parser SomeExpr]
operators =
    [ binOp "+"  EAdd
    , binOp "-"  ESub
    , binOp "*"  EMul
    ] ++
    [ binOp "/s" (EQuot Signed)
    , binOp "/u" (EQuot Unsigned)
    , binOp "%s" (ERem Signed)
    , binOp "%u" (ERem Unsigned)
    ] ++
    [ binOp "&"  EAnd
    , binOp "|"  EOr
    , binOp "^"  EXOr
    , unOp  "~"  ENot
    , shOp "<<"  EShl
    , shOp ">>l" EShrl
    , shOp ">>a" EShra
    , unOp  "-"  ENegate
    ] ++
    narrowOps ++
    extendOps "sext" ESignExt ++
    extendOps "zext" EZeroExt ++
    map relOp allRelationalOps ++
    loadOps
  where
    checkWidths :: forall a b. (KnownWidth a, KnownWidth b) => Except String (a :~: b)
    checkWidths
      | SameWidth <- Proxy @a `compareWidths` Proxy @b
      = return Refl
      | otherwise = throwE $ unwords ["type mismatch:", show (knownWidth @a), show (knownWidth @b)]

    runExcept' :: Except String a -> a
    runExcept' = either error id . runExcept

    homo :: forall r. ()
         => (forall w. (KnownWidth w) => Expr w -> Expr w -> r)
         -> SomeExpr -> SomeExpr -> r
    homo f a b = runExcept' $ do
        SomeExpr (ea :: Expr wa) <- pure a
        SomeExpr (eb :: Expr wb) <- pure b
        Refl <- checkWidths @wa @wb
        return (f ea eb)

    check1 :: forall w r. (KnownWidth w)
           => (Expr w -> r)
           -> SomeExpr -> r
    check1 f a = runExcept' $ do
        SomeExpr (ea :: Expr wa) <- pure a
        Refl <- checkWidths @wa @w
        return (f ea)

    binOp :: String
          -> (forall w. (KnownWidth w) => Expr w -> Expr w -> Expr w)
          -> Operator Parser SomeExpr
    binOp op f = InfixN (homo (\x y -> SomeExpr $ f x y) <$ symbol op)

    unOp :: String
         -> (forall w. (KnownWidth w) => Expr w -> Expr w)
         -> Operator Parser SomeExpr
    unOp op f = Prefix (g <$ symbol op)
      where
        g (SomeExpr e) = SomeExpr (f e)

    -- shift operation
    shOp
        :: String
        -> (forall w. (KnownWidth w) => Expr w -> Expr WordSize -> Expr w)
        -> Operator Parser SomeExpr
    shOp tok f = InfixN $ do
        _ <- symbol tok
        return $ \a b -> runExcept' $ do
            SomeExpr (ea :: Expr wa) <- pure a
            SomeExpr (eb :: Expr wb) <- pure b
            Refl <- checkWidths @wb @WordSize
            return $ SomeExpr (f ea eb)

    loadOps :: [Operator Parser SomeExpr]
    loadOps = forAllWidths f
      where
        f :: forall width. (KnownWidth width)
          => Proxy width -> Operator Parser SomeExpr
        f _ = Prefix $ do
            _ <- symbol ("load[" ++ show (knownWidth @width) ++ "]")
            return $ check1 @WordSize $ SomeExpr . ELoad @width

    relOp :: RelationalOp -> Operator Parser SomeExpr
    relOp op = InfixN $ do
        _ <- symbol (relationalOpString op)
        return $ homo $ \x y -> SomeExpr $ ERel op x y

    fixedWidthUnOp
        :: forall w w'. (KnownWidth w, KnownWidth w')
        => String
        -> (Expr w -> Expr w')
        -> Operator Parser SomeExpr
    fixedWidthUnOp tok f = Prefix $ do
        _ <- symbol tok
        return $ check1 @w $ SomeExpr . f

    narrowOps :: [Operator Parser SomeExpr]
    narrowOps = do
        SomeWidth (narrow :: Proxy narrow) <- allWidths
        SomeWidth (wide :: Proxy wide) <- allWidths
        Wider <- pure $ wide `compareWidths` narrow
        let tok = concat ["narrow[", show (knownWidth @wide), "→", show (knownWidth @narrow), "]"]
        return $ fixedWidthUnOp tok (ENarrow @wide @narrow)

    extendOps
        :: String
        -> (forall wide narrow. (KnownWidth narrow, wide `WiderThan` narrow) => Expr narrow -> Expr wide)
        -> [Operator Parser SomeExpr]
    extendOps op k = do
        SomeWidth (narrow :: Proxy narrow) <- allWidths
        SomeWidth (wide :: Proxy wide) <- allWidths
        Wider <- pure $ wide `compareWidths` narrow
        let tok = concat [op, "[", show (knownWidth @narrow), "→", show (knownWidth @wide), "]"]
        return $ fixedWidthUnOp tok (k @wide @narrow)

lit :: Parser SomeExpr
lit = choice $ forAllWidths f
  where
    hex, decimal :: Parser Integer
    hex = try $ string "0x" >> L.hexadecimal
    decimal = L.decimal

    f :: forall width. (KnownWidth width)
      => Proxy width -> Parser SomeExpr
    f _ = try $ do
        n <- hex <|> decimal
        _ <- symbol $ "::" <> show (knownWidth @width)
        return $ SomeExpr $ ELit $ fromUnsigned @width $ fromIntegral n

spaceC :: Parser ()
spaceC = L.space space1 mzero mzero

symbol :: String -> Parser String
symbol = L.symbol spaceC

parens :: Parser SomeExpr -> Parser SomeExpr
parens = between (symbol "(") (symbol ")")

prop_roundtrips
    :: SomeExpr -> Property
prop_roundtrips (SomeExpr e) =
    case parseSomeExpr (show e) of
      SomeExpr e' ->
        case e `compareWidths` e' of
          SameWidth -> show e' === show e
          _         -> property False
