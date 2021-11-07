-- | Cmm pipeline correctness testsuite.
module Main where

import Data.Proxy
import Data.Tagged
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm
import Interpreter
import CallishOp
import CCall
import RunGhc

compilerConfigs :: FilePath -> [(String, Compiler)]
compilerConfigs ghcPath =
    [ ("o0-ncg",  Compiler ghcPath (["-O0"] ++ commonArgs))
    , ("o1-ncg",  Compiler ghcPath (["-O1"] ++ commonArgs))
    , ("o0-llvm", Compiler ghcPath (["-O0", "-fllvm"] ++ commonArgs))
    , ("o1-llvm", Compiler ghcPath (["-O1", "-fllvm"] ++ commonArgs))
    ]
  where
    commonArgs = ["-dcmm-lint", "-dasm-lint"]

-- * Properties

expr_prop :: Compiler -> Expr W64 -> Property
expr_prop comp e = conjoin
    [ converges refInterpreter e
    , agree refInterpreter (ghcDynInterpreter' comp) e
    ]

quotRemProp
    :: (KnownWidth width)
    => Interpreter WordSize
    -> Signedness
    -> Number width            -- ^ dividend
    -> NonZero (Number width)  -- ^ divisor
    -> Property
quotRemProp interp s a (NonZero b) = ioProperty $ do
    r <- interp (ERel REq x rhs)
    return $ r === 1
  where
    x = ELit a
    y = ELit b
    rhs = ((EQuot s x y) * y) + ERem s x y

compilerTests :: String -> Compiler -> TestTree
compilerTests name comp = testGroup name
    [ testProperty "expression correctness" (expr_prop comp)
    , testProperty "callish correctness" (prop_callish_ops_correct comp)
    , testProperty "C-Call correctness" (testCCall comp)
    , testGroup "Quot-Rem invariant"
      [ testProperty (show (knownWidth @w))
        $ quotRemProp @w (ghcDynInterpreter' comp)
      | SomeWidth (_ :: Proxy w) <- allWidths
      ]
    ]

newtype GhcPath = GhcPath FilePath
instance IsOption GhcPath where
    defaultValue = GhcPath "ghc"
    parseValue = Just . GhcPath
    optionName = Tagged "ghc-path"
    optionHelp = Tagged "Path to compiler to test"

main :: IO ()
main = do
    createBufferFile
    let ing = defaultIngredients ++ [includingOptions [Option (Proxy @GhcPath)]]
    defaultMainWithIngredients ing
        $ askOption $ \(GhcPath ghcPath) ->
          testGroup "primops"
        [ compilerTests name comp
        | (name, comp) <- compilerConfigs ghcPath
        ]
