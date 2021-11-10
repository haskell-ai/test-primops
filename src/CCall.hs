-- | Correctness test for C calling convention.
module CCall
    ( CCallDesc(..)
    , testCCall
    ) where

import Numeric.Natural
import System.FilePath
import System.IO.Temp
import Test.QuickCheck

import Expr
import Width
import ToCmm
import RunGhc
import Number

data CCallDesc
    = CCallDesc { callRet :: SomeNumber
                , callArgs :: [SomeNumber]
                }
    deriving (Show)

retWidth :: CCallDesc -> Width
retWidth = someNumberWidth . callRet

mAX_ARGS :: Int
mAX_ARGS = 32

instance Arbitrary CCallDesc where
    arbitrary = do
        ret <- arbitrary
        n <- chooseInt (0, mAX_ARGS)
        args <- vectorOf n arbitrary
        return $ CCallDesc ret args

testCCall
    :: Compiler
    -> CCallDesc
    -> Property
testCCall comp c = 
    ioProperty $ withTempDirectory "." "tmp" $ \tmpDir -> do
        writeFile (tmpDir </> "test_c.c") (cStub c)
        writeFile (tmpDir </> "test.cmm") (cCallCmm c)
        compile comp tmpDir ["test_c.c", "test.cmm"] soName ["-shared", "-dynamic"]
        out <- runIt comp (tmpDir </> soName)
        let saw :: [Natural]
            saw = map read (lines out)
            expected :: [Natural]
            expected = map (\(SomeNumber e) -> toUnsigned e) (callArgs c) ++ [ret]
            ret = case callRet c of SomeNumber n -> toUnsigned n
        return $ saw === expected

  where
    soName = "test.so"

cStub :: CCallDesc -> String
cStub c
  = unlines
    [ "#include <stdio.h>"
    , "#include <stdint.h>"
    , "#include <inttypes.h>"
    , ""
    , funcDef
    ]
  where
    argBndrs = [ "arg"++show i | (i,_) <- zip [0::Int ..] (callArgs c) ]
    argWidths = [ knownWidth @w | SomeNumber (_ :: Number w) <- callArgs c ]

    funcDef = unlines $
      [ cType (retWidth c) <> " test_c(" <> argList <> ") {" ] ++
      zipWith printArg argWidths argBndrs ++
      [ "  fflush(stdout);"
      , "  return " ++ show (someNumberToUnsigned $ callRet c) ++ "ULL;"
      , "}"
      ]

    argList =
        commaList
        [ unwords [cType w, bndr]
        | (w, bndr) <- zip argWidths argBndrs
        ]
    printArg w bndr =
        "  printf(" ++ quoted (formatStr w ++ "\\n") ++ ", " ++ bndr ++ ");"

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

formatStr :: Width -> String
formatStr w =
    "0x%" ++ quoted ("PRIx"++show n)
  where
    n = widthBits w

cType :: Width -> String
cType W8  = "uint8_t"
cType W16 = "uint16_t"
cType W32 = "uint32_t"
cType W64 = "uint64_t"

cCallCmm :: CCallDesc -> String
cCallCmm c = unlines
    [ "test(bits64 buffer) {"
    , "  "++cmmType (retWidth c)++" ret;"
    , "  (ret) = foreign \"C\" test_c(" ++ argList ++ ");"
    , "  return (%zx64(ret));"
    , "}"
    ]
  where
    argList =
        commaList
        [ exprToCmm $ ELit e
        | SomeNumber e <- callArgs c
        ]
