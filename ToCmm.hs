module ToCmm
    ( createBufferFile
    , evalGhc
    , evalGhcDyn
    , toCmmDecl
    , toCmmExpr
    ) where

import qualified Data.ByteString as BS
import System.Exit
import System.Process
import System.IO.Temp
import System.FilePath
import Data.List (intercalate)
import Numeric.Natural

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
    [ name <> " ( bits64 buffer )"
    , "{"
    , "  bits64 ret;"
    , "  ret = " <> toCmmExpr e <> ";"
    , "  return (ret);"
    , "}"
    ]

narrowOp, zeroExtOp, signExtOp :: Width -> String
narrowOp w = "%lobits" <> show (widthBits w)
zeroExtOp w = "%zx" <> show (widthBits w)
signExtOp w = "%sx" <> show (widthBits w)

machOp :: String -> [String] -> String
machOp op args = op <> parens (intercalate "," args)

toCmmExpr :: forall width. KnownWidth width => Expr width -> String
toCmmExpr e =
    case e of
      EAdd    a b -> binOp "+" a b
      ESub    a b -> binOp "-" a b
      EMul    a b -> binOp "*" a b
      EDivU   a b -> machOp "%divu"       [toCmmExpr a, toCmmExpr b]
      ERemU   a b -> machOp "%modu"       [toCmmExpr a, toCmmExpr b]
      EDivS   a b -> machOp "%rem"        [toCmmExpr a, toCmmExpr b]
      ERemS   a b -> machOp "%quot"       [toCmmExpr a, toCmmExpr b]
      EAnd    a b -> binOp "&" a b
      EOr     a b -> binOp "|" a b
      ENot    a   -> parens $ "~" <> toCmmExpr a
      EShl    a b -> machOp "%shl"        [toCmmExpr a, toCmmExpr b]
      EShrl   a b -> machOp "%shrl"       [toCmmExpr a, toCmmExpr b]
      EShra   a b -> machOp "%shra"       [toCmmExpr a, toCmmExpr b]
      ENegate a   -> machOp "%neg"        [toCmmExpr a]
      ENarrow a   -> machOp (narrowOp  w) [toCmmExpr a]
      ESignExt a  -> machOp (signExtOp w) [toCmmExpr a]
      EZeroExt a  -> machOp (zeroExtOp w) [toCmmExpr a]
      ELoad off   -> cmmType w <> braces ("buffer + " <> toCmmExpr off)
      ELit n      -> parens $ unwords [show (getNumber n), "::", cmmType (knownWidth @width)]
  where
    binOp op a b = parens $ unwords [toCmmExpr a, op, toCmmExpr b]
    w = knownWidth @width

parens, braces :: String -> String
parens s = concat ["(", s, ")"]
braces s = concat ["[", s, "]"]

createBufferFile :: IO ()
createBufferFile = do
    BS.writeFile "test" buffer

evalGhc :: forall width. (KnownWidth width) => Expr width -> IO Natural
evalGhc e = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> hsSrc) $ unlines
        [ "{-# LANGUAGE GHCForeignImportPrim #-}"
        , "{-# LANGUAGE UnliftedFFITypes #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "module Main where"
        , "import Data.Word"
        , "import GHC.Exts"
        , "import GHC.Ptr (Ptr(Ptr))"
        , "import System.IO.MMap"
        , "foreign import prim \"test\" test :: Addr# -> " <> hsType w
        , "main :: IO ()"
        , "main = do"
        , "  (Ptr p, _, _, _) <- mmapFilePtr \"test\" ReadOnly Nothing"
        , "  print $ " <> toHsWord w "test p"
        ]
    writeFile (tmpDir </> cmmSrc) $ toCmmDecl "test" e
    let inTmp c = c { cwd = Just tmpDir }
    let ghcArgs = ["-O0", "-dcmm-lint"]
    runProcess' $ inTmp (proc ghcPath $ ghcArgs ++ [hsSrc, cmmSrc, "-o", exeName])
    out <- readProcess (tmpDir </> exeName) [] ""
    return $ read out
  where
    w = knownWidth @width
    runProcess' p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()
    exeName = "Test"
    cmmSrc = "test-cmm.cmm"
    hsSrc = "test-hs.hs"

evalGhcDyn :: forall width. (KnownWidth width) => Expr width -> IO Natural
evalGhcDyn e = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> cmmSrc) $ toCmmDecl "test" e
    let inTmp c = c { cwd = Just tmpDir }
    let ghcArgs = ["-O0", "-package-env", "-", "-dcmm-lint"]
    runProcess' $ inTmp (proc ghcPath $ ghcArgs ++ ["-shared", "-o", soName, cmmSrc])
    out <- readProcess runnerName [tmpDir </> soName] ""
    return $ read out
  where
    runnerName = "run-it"
    runProcess' p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()
    soName = "Test.so"
    cmmSrc = "test-cmm.cmm"
