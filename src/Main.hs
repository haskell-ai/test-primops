-- | Cmm pipeline correctness testsuite.
module Main
    ( module Main
    , parseExpr
    ) where

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
import Expr.Parse

basicCompiler :: FilePath -> Compiler
basicCompiler ghcPath =
    Compiler { compPath = ghcPath
             , compArgs = ["-dcmm-lint", "-dasm-lint", "-O0"]
             , compRunIt = "run-it"
             }

compilerConfigs :: FilePath -> [(String, Compiler)]
compilerConfigs ghcPath =
    [ ("o0-ncg",  c0 `addArgs` ["-O0"])
    , ("o1-ncg",  c0 `addArgs` ["-O1"])
    , ("o0-llvm", c0 `addArgs` ["-O0", "-fllvm"])
    , ("o1-llvm", c0 `addArgs` ["-O1", "-fllvm"])
    ]
  where
    c0 = basicCompiler ghcPath

-- * Properties

expr_prop :: Compiler -> Expr WordSize -> Property
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
    , prop_callish_ops_correct comp
    , testProperty "C-Call correctness" (testCCall comp)
    , testGroup "Quot-Rem invariant"
      [ testProperty (show (knownWidth @w))
        $ quotRemProp @w (ghcDynInterpreter' comp)
      | SomeWidth (_ :: Proxy w) <- allWidths
      ]
    ]

newtype RunItPath = RunItPath FilePath
instance IsOption RunItPath where
    defaultValue = RunItPath "run-it"
    parseValue = Just . RunItPath
    optionName = Tagged "run-it-path"
    optionHelp = Tagged "Path to the run-it executable compiled with the compiler-under-test"

newtype GhcPath = GhcPath FilePath
instance IsOption GhcPath where
    defaultValue = GhcPath "ghc"
    parseValue = Just . GhcPath
    optionName = Tagged "ghc-path"
    optionHelp = Tagged "Path to compiler to test"

runCompilerTests :: Compiler -> IO ()
runCompilerTests = defaultMain . compilerTests "compiler"

main :: IO ()
main = do
    createBufferFile
    let ing = defaultIngredients ++ [includingOptions [Option (Proxy @GhcPath), Option (Proxy @RunItPath)]]
    defaultMainWithIngredients ing
        $ askOption $ \(GhcPath ghcPath) ->
          askOption $ \(RunItPath runItPath) ->
          testGroup "primops"
        [ compilerTests name comp'
        | (name, comp) <- compilerConfigs ghcPath
        , let comp' = comp { compRunIt = runItPath }
        ]
