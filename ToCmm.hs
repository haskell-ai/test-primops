module ToCmm
    ( createBufferFile
    , cmmType
    , evalGhc
    , evalGhcDyn
    , evalCmm
    , toCmmDecl
    , exprToCmm
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
commaList = intercalate ","

createBufferFile :: IO ()
createBufferFile = do
    BS.writeFile "test" buffer

compile :: FilePath   -- ^ working directory
        -> [FilePath] -- ^ sources
        -> FilePath   -- ^ output path
        -> [String]   -- ^ other arguments
        -> IO ()
compile workDir srcs out args = do
    runProcess' $ inTmp (proc ghcPath $ srcs ++ args ++ ["-o", out])
  where
    inTmp c = c { cwd = Just workDir }
    runProcess' p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()

evalGhc :: forall width. (KnownWidth width)
        => [String] -> Expr width -> IO Natural
evalGhc ghcArgs e = withTempDirectory "." "tmp" $ \tmpDir -> do
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
    compile tmpDir [cmmSrc, hsSrc] exeName ghcArgs
    out <- readProcess (tmpDir </> exeName) [] ""
    return $ read out
  where
    w = knownWidth @width
    exeName = "Test"
    cmmSrc = "test-cmm.cmm"
    hsSrc = "test-hs.hs"

evalGhcDyn :: forall width. (KnownWidth width)
           => [String] -> Expr width -> IO Natural
evalGhcDyn ghcArgs e = evalCmm ghcArgs $ toCmmDecl "test" e

type Cmm = String

-- | Evaluate a Cmm function. Must be named @test@.
evalCmm :: [String] -> Cmm -> IO Natural
evalCmm ghcArgs cmm = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> cmmSrc) cmm
    let ghcArgs' = ghcArgs ++ ["-dynamic", "-package-env", "-", "-shared"]
    compile tmpDir [cmmSrc] soName ghcArgs'
    out <- readProcess runnerName [tmpDir </> soName] ""
    return $ read out
  where
    runnerName = "run-it"
    soName = "Test.so"
    cmmSrc = "test-cmm.cmm"
