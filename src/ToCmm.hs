-- | Tools for rendering 'Expr's to Cmm.
module ToCmm
    ( createBufferFile
    , cmmType
    , cmmWordType
    , toCmmDecl
    , exprToCmm
      -- * Utilities
    , commaList
    , parens
    ) where

import qualified Data.ByteString as BS
import Data.List (intercalate)

import Width
import Number
import Expr

cmmType :: Width -> String
cmmType W8  = "bits8"
cmmType W16 = "bits16"
cmmType W32 = "bits32"
cmmType W64 = "bits64"

cmmWordType :: String
cmmWordType = cmmType $ knownWidth @WordSize

toCmmDecl :: KnownWidth width => String -> Expr width -> String
toCmmDecl name e = unlines
    [ name <> " ( " <> cmmWordType <> " buffer )"
    , "{"
    , "  " <> cmmWordType <> " ret;"
    , "  ret = " <> exprToCmm e <> ";"
    , "  return (ret);"
    , "}"
    ]

narrowOp, zeroExtOp, signExtOp :: Width -> String
narrowOp w = "%lobits" <> show (widthBits w)
zeroExtOp w = "%zx" <> show (widthBits w)
signExtOp w = "%sx" <> show (widthBits w)

machOp :: String -> [String] -> String
machOp op args = op <> parens (commaList args)

cmmRelOp :: RelationalOp -> String
cmmRelOp op =
    case op of
      REq    -> "%eq"
      RNEq   -> "%ne"
      RGT  s -> signed s "%gt"
      RGE  s -> signed s "%ge"
      RLT  s -> signed s "%lt"
      RLE  s -> signed s "%le"
  where
    signed Signed = id
    signed Unsigned = (++"u")

exprToCmm :: forall width. KnownWidth width => Expr width -> String
exprToCmm e =
    case e of
      ERel op a b -> machOp (cmmRelOp op) [exprToCmm a, exprToCmm b]
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EMul    a b -> binOp "*" a b
      EQuot s a b -> machOp (quotOp s)    [exprToCmm a, exprToCmm b]
      ERem  s a b -> machOp (remOp s)     [exprToCmm a, exprToCmm b]
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      EXOr    a b -> binOp "^" a b
      ENot    a   -> parens $ "~" <> exprToCmm a
      EShl    a b -> machOp "%shl"        [exprToCmm a, exprToCmm b]
      EShrl   a b -> machOp "%shrl"       [exprToCmm a, exprToCmm b]
      EShra   a b -> machOp "%shra"       [exprToCmm a, exprToCmm b]
      ENegate a   -> machOp "%neg"        [exprToCmm a]
      ENarrow a   -> machOp (narrowOp  w) [exprToCmm a]
      ESignExt a  -> machOp (signExtOp w) [exprToCmm a]
      EZeroExt a  -> machOp (zeroExtOp w) [exprToCmm a]
      ELoad off   -> cmmType w <> braces ("buffer + " <> exprToCmm off)
      ELit n      -> parens $ unwords [show (toSigned n), "::", cmmType (knownWidth @width)]
  where
    binOp op a b = parens $ unwords [exprToCmm a, op, exprToCmm b]
    w = knownWidth @width
    quotOp Unsigned = "%divu"
    quotOp Signed   = "%quot"
    remOp  Unsigned = "%modu"
    remOp  Signed   = "%rem"

parens, braces :: String -> String
parens s = concat ["(", s, ")"]
braces s = concat ["[", s, "]"]

commaList :: [String] -> String
commaList = intercalate ", "

createBufferFile :: IO ()
createBufferFile = do
    BS.writeFile "test" buffer

