module ToCmm where

import Width
import Number
import Expr

cmmType :: Width -> String
cmmType W8  = "bits8"
cmmType W16 = "bits16"
cmmType W32 = "bits32"
cmmType W64 = "bits64"

toCmmDecl :: KnownWidth width => String -> Expr width -> String
toCmmDecl name e = unlines
    [ "#include \"Cmm.h\""
    , name <> " ( W_ arg1 )"
    , "{"
    , "  W_ ret;"
    , "  ret = " <> toCmmExpr e <> ";"
    , "  return (ret);"
    , "}"
    ]

toCmmExpr :: forall width. KnownWidth width => Expr width -> String
toCmmExpr e =
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
      ELit n -> parens $ unwords [show (getNumber n), "::", cmmType (knownWidth @width)]
  where
    binOp op a b = parens $ unwords [showExpr a, op, showExpr b]
    parens s = concat ["(", s, ")"]
