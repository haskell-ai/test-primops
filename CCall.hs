module CCall where

import Numeric.Natural
import System.FilePath
import System.IO.Temp
import Test.QuickCheck

import Width
import ToCmm
import Number
import Expr

data CCallDesc 
    = CCallDesc { callRet :: SomeNumber
                , callArgs :: [SomeNumber]
                }
    deriving (Show)

mAX_ARGS :: Int
mAX_ARGS = 32

instance Arbitrary CCallDesc where
    arbitrary = do
        ret <- arbitrary
        n <- chooseInt (0, mAX_ARGS)
        args <- vectorOf n arbitrary
        return $ CCallDesc ret args

testCCall
    :: FilePath  -- ^ GHC path
    -> CCallDesc
    -> Property
testCCall ghcPath c = 
    ioProperty $ withTempDirectory "." "tmp" $ \tmpDir -> do
        writeFile (tmpDir </> "test_c.c") (cStub c)
        writeFile (tmpDir </> "test.cmm") (cCallCmm c)
        compile ghcPath tmpDir ["test_c.c", "test.cmm"] soName ["-shared", "-dynamic"]
        out <- runIt (tmpDir </> soName)
        putStrLn out
        let saw :: [Natural]
            saw = map read (lines out)
            expected :: [Natural]
            expected = map (\(SomeNumber e) -> toUnsigned e) (callArgs c) ++ [ret]
            ret = case callRet c of SomeNumber n -> toUnsigned n
        return $ saw === expected

  where
    soName = "test.so"

cStub :: CCallDesc -> String
cStub c@(CCallDesc { callRet = SomeNumber (ret :: Number ret) })
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
    retType = cType (knownWidth @ret)

    funcDef = unlines $
      [ retType <> " test_c(" <> argList <> ") {" ] ++
      zipWith printArg argWidths argBndrs ++
      [ "  fflush(stdout);"
      , "  return " ++ show ret ++ ";"
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
    , "  bits64 ret;"
    , "  (ret) = foreign \"C\" test_c(" ++ argList ++ ");"
    , "  return (%zx64(ret));"
    , "}"
    ]
  where
    argList =
        commaList
        [ show e
        | SomeNumber e <- callArgs c
        ]
