module Main where

import Data.Proxy
import Test.QuickCheck
import Prelude hiding (truncate)

import Width
import Number
import Expr
import ToCmm
import TestUtils
import CallishOp
import CCall
import RunGhc

gHC_PATH :: FilePath
gHC_PATH = "/opt/exp/ghc/ghc-8.10/_build/stage1/bin/ghc"

ghc, ghcLlvm :: Compiler
ghc = Compiler gHC_PATH ["-O0", "-dcmm-lint", "-dasm-lint"]
ghcLlvm = Compiler gHC_PATH ["-fllvm", "-O0", "-dcmm-lint", "-dasm-lint"]

ghcInterpreter :: Interpreter W64
ghcInterpreter = ghcDynInterpreter' ghc

-- * Properties

expr_prop :: Compiler -> Expr W64 -> Property
expr_prop comp e = conjoin
    [ interpreterConverges refInterpreter e
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
    createBufferFile
    quickCheck $ verbose (compiler_prop ghc)
    return ()
