-- | Cmm pipeline correctness testsuite.
module Main where

import Data.Proxy
import Test.QuickCheck
import System.Environment (getArgs)
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm
import Interpreter
import CallishOp
import CCall
import RunGhc

gHC_PATH :: FilePath
gHC_PATH = "/opt/exp/ghc/ghc-8.10/_build/stage1/bin/ghc"

compilerConfigs :: FilePath -> [Compiler]
compilerConfigs ghcPath =
    [ Compiler ghcPath (["-O0"] ++ commonArgs)
    , Compiler ghcPath (["-O1"] ++ commonArgs)
    , Compiler ghcPath (["-O1", "-fllvm"] ++ commonArgs)
    ]
  where
    commonArgs = ["-dcmm-lint", "-dasm-lint"]

-- * Properties

expr_prop :: Compiler -> Expr W64 -> Property
expr_prop comp e = conjoin
    [ converges refInterpreter e
    , agree refInterpreter (ghcDynInterpreter' comp) e
    ]

compiler_prop :: Compiler -> Property
compiler_prop comp = conjoin
    [ property (expr_prop comp)
    , prop_callish_ops_correct comp
    , property $ testCCall comp
    , conjoin [ property $ quotRemProp @w (ghcDynInterpreter' comp)
              | SomeWidth (_ :: Proxy w) <- allWidths
              ]
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

main :: IO ()
main = do
    [ghcPath] <- getArgs
    createBufferFile
    quickCheck $ verbose $ conjoin $ map compiler_prop (compilerConfigs ghcPath)
    return ()
