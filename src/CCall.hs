-- | Correctness test for C calling convention.
module CCall
    ( CCallDesc(..)
    , testCCall
    ) where

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
                , callRetSignedness :: Signedness
                , callArgs :: [(Signedness, SomeNumber)]
                }
    deriving (Show)

retWidth :: CCallDesc -> Width
retWidth = someNumberWidth . callRet

mAX_ARGS :: Int
mAX_ARGS = 32

instance Arbitrary CCallDesc where
    arbitrary = do
        ret <- arbitrary
        ret_signedness <- arbitrary
        n <- chooseInt (0, mAX_ARGS)
        args <- vectorOf n arbitrary
        return $ CCallDesc ret ret_signedness args
    shrink (CCallDesc ret ret_s args) =
        CCallDesc <$> shrink ret <*> pure ret_s <*> shrinkList shrink args

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
        let saw :: [Integer]
            saw = map read (lines out)
            expected :: [Integer]
            expected = map (\(s, SomeNumber e) -> asInteger s e) (callArgs c) ++ [ret]
            -- The wrapper zero extends the result so interpret it as unsigned.
            ret = case callRet c of SomeNumber n -> asInteger Unsigned n
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
    argTypes =
      [ (signedness, knownWidth @w)
      | (signedness, SomeNumber (_ :: Number w)) <- callArgs c
      ]

    funcDef = unlines $
      [ cType Unsigned (retWidth c) <> " test_c(" <> argList <> ") {" ] ++
      zipWith printArg argTypes argBndrs ++
      [ "  fflush(stdout);"
      , "  return " ++ show (someNumberToUnsigned $ callRet c) ++ "ULL;"
      , "}"
      ]

    argList =
        commaList
        [ unwords [ty, bndr]
        | (ty, bndr) <- zip (map (uncurry cType) argTypes) argBndrs
        ]
    printArg ty bndr =
        "  printf(" ++ quoted (formatStr ty ++ "\\n") ++ ", " ++ bndr ++ ");"

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

formatStr :: (Signedness, Width) -> String
formatStr (signedness, w) =
    "%" ++ quoted ("PRI" ++ fmt ++ show n)
  where
    fmt = case signedness of
            Signed   -> "d"
            Unsigned -> "u"
    n = widthBits w

cType :: Signedness -> Width -> String
cType signedness width  = prefix ++ "int" ++ show n ++ "_t"
  where
    n = widthBits width
    prefix = case signedness of
      Signed   -> ""
      Unsigned -> "u"

cCallCmm :: CCallDesc -> String
cCallCmm c = unlines
    [ "test("++cmmWordType ++" buffer) {"
    , "  "++cmmType (retWidth c)++" ret;"
    , "  (" ++ retHint ++ "ret) = foreign \"C\" test_c(" ++ argList ++ ");"
    , "  return ("++widenOp++"(ret));"
    , "}"
    ]
  where
    retHint = case callRetSignedness c of
                Signed   -> "\"signed\" "
                Unsigned -> ""
    widenOp = "%zx" ++ show (widthBits wordSize)
    argList =
        commaList
        [ exprToCmm (ELit e) ++ hint
        | (signedness, SomeNumber e) <- callArgs c
        , let hint = case signedness of
                       Signed -> " \"signed\""
                       Unsigned -> ""
        ]
