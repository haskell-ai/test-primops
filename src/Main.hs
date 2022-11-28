-- | Cmm pipeline correctness testsuite.
module Main
    ( module Main
    , parseExpr
    ) where

import Data.Proxy
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.QuickCheck
import Options.Applicative
import Prelude hiding (truncate)

import CCall
import CallishOp
import Compiler
import Expr
import Expr.Parse
import Interpreter
import MulMayOverflow
import Number
import RunGhc
import ToCmm
import Width

newtype UsedEvalMethod = UsedEvalMethod EvalMethod

newtype GhcPath = GhcPath FilePath

instance IsOption GhcPath where
    defaultValue = GhcPath "ghc"
    parseValue = pure . GhcPath
    optionName = return "ghc-path"
    optionHelp = return "Path to compiler"
    optionCLParser =
        GhcPath <$> option str (long "ghc-path" <> help "Path to compiler")

newtype GhcArgs = GhcArgs [String]

instance IsOption GhcArgs where
    defaultValue = GhcArgs []
    parseValue = pure . GhcArgs . words
    optionName = return "ghc-args"
    optionHelp = return "Arguments to pass to GHC"
    optionCLParser =
        fmap GhcArgs $ some $ option str (long "ghc-args" <> help "Arguments to pass to GHC")

newtype RunItPath = RunItPath FilePath

instance IsOption RunItPath where
    defaultValue = RunItPath "run-it"
    parseValue = pure . RunItPath
    optionName = return "run-it-path"
    optionHelp = return "Path to run-it executable built with tested compiler"
    optionCLParser =
        RunItPath <$> option str (long "run-it-path" <> help "Path to run-it")

newtype Emulator = Emulator (Maybe FilePath)

instance IsOption Emulator where
    defaultValue = Emulator Nothing
    parseValue = pure . Emulator . Just
    optionName = return "emulator"
    optionHelp = return "Path to emulator to use to run target executables"
    optionCLParser =
        Emulator . Just <$> option str (long "emulator" <> help "Path to emulator executable")

basicCompiler :: FilePath -> Compiler
basicCompiler ghcPath =
    Compiler { compPath = ghcPath
             , compArgs = ["-dcmm-lint", "-dasm-lint", "-O0"]
             }

-- * Properties

expr_prop :: EvalMethod -> Expr WordSize -> Property
expr_prop em e = conjoin
    [ converges refInterpreter e
    , agree refInterpreter (ghcInterpreter em) e
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

compilerTests :: EvalMethod -> [TestTree]
compilerTests em =
    [ testProperty "expression correctness" (expr_prop em)
    , prop_callish_ops_correct em
    , testProperty "C-Call correctness" (testCCall em)
    , testGroup "Quot-Rem invariant"
      [ testProperty (show (knownWidth @w))
        $ quotRemProp @w (ghcInterpreter em)
      | SomeWidth (_ :: Proxy w) <- allWidths
      ]
    , prop_mul_may_oflo_correct em
    ]

runCompilerTests :: Compiler -> IO ()
runCompilerTests =
    defaultMain . testGroup "compiler" . compilerTests  . staticEvalMethod

main :: IO ()
main = do
    createBufferFile
    let opts = [ Option (Proxy @GhcPath)
               , Option (Proxy @GhcArgs)
               , Option (Proxy @RunItPath)
               , Option (Proxy @Emulator)
               ]
        ing = defaultIngredients ++ [includingOptions opts]
    defaultMainWithIngredients ing
        $ askOption $ \(GhcPath ghcPath) ->
          askOption $ \(GhcArgs ghcArgs) ->
          askOption $ \(RunItPath runItPath) ->
          askOption $ \(Emulator mbEmulator) ->
              let comp = Compiler ghcPath ghcArgs
                  em = case mbEmulator of
                         Just emulator -> emulatedStaticEvalMethod comp emulator
                         Nothing -> DynamicEval comp runItPath
              in testGroup "test-primops" $ compilerTests em
