module ToCmm where

import System.Exit
import System.Process
import System.IO.Temp
import System.FilePath

import Width
import Number
import Expr

ghcPath :: FilePath
ghcPath = "ghc"

hsType :: Width -> String
hsType W8  = "Word8#"
hsType W16 = "Word16#"
hsType W32 = "Word32#"
hsType W64 = "Word#"

toHsWord :: Width -> String -> String
toHsWord w x = "W# " <> parens (extendFn <> " " <> parens x)
  where
    extendFn
      | w == W64  = ""
      | otherwise =  "extendWord" <> show (widthBits w) <> "#"

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

narrowOp, zeroExtOp, signExtOp :: Width -> String
narrowOp w = "%lobits" <> show (widthBits w)
zeroExtOp w = "%zx" <> show (widthBits w)
signExtOp w = "%sx" <> show (widthBits w)

toCmmExpr :: forall width. KnownWidth width => Expr width -> String
toCmmExpr e =
    case e of
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      ENot    a   -> parens $ "~" <> toCmmExpr a
      EShl    a b -> binOp "<<" a b
      EShr    a b -> binOp ">>" a b
      ENegate a   -> parens $ "-" <> toCmmExpr a
      ENarrow (a :: Expr wide)    -> narrowOp  (knownWidth @width) <> parens (toCmmExpr a)
      ESignExt (a :: Expr narrow) -> signExtOp (knownWidth @width) <> parens (toCmmExpr a)
      EZeroExt (a :: Expr narrow) -> zeroExtOp (knownWidth @width) <> parens (toCmmExpr a)
      ELit n -> parens $ unwords [show (getNumber n), "::", cmmType (knownWidth @width)]
  where
    binOp op a b = parens $ unwords [toCmmExpr a, op, toCmmExpr b]

parens :: String -> String
parens s = concat ["(", s, ")"]

evalGhc :: forall width. (KnownWidth width) => Expr width -> IO Integer
evalGhc e = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> hsSrc) $ unlines
        [ "{-# LANGUAGE GHCForeignImportPrim #-}"
        , "{-# LANGUAGE UnliftedFFITypes #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "module Main where"
        , "import Data.Word"
        , "import GHC.Exts"
        , "foreign import prim \"test\" test :: Word# -> " <> hsType w
        , "main :: IO ()"
        , "main = print $ " <> toHsWord w "test 0##"
        ]
    writeFile (tmpDir </> cmmSrc) $ toCmmDecl "test" e
    let inTmp c = c { cwd = Just tmpDir }
    runProcess $ inTmp (proc ghcPath [hsSrc, cmmSrc, "-v0", "-o", exeName])
    out <- readProcess (tmpDir </> exeName) [] ""
    return $ read out
  where
    w = knownWidth @width
    runProcess p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()
    exeName = "Test"
    cmmSrc = "test-cmm.cmm"
    hsSrc = "test-hs.hs"
