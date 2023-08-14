{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler
    ( Compiler(..)
    , basicCompiler
    , addArgs
    , compile
      -- * Compiling objects
    , compileC
    , compileCmm
    , compileHs
      -- * Test programs
    , TestProgram
    , writeObjectsIn
    ) where

import qualified Data.ByteString as BS
import System.Exit
import System.Process
import System.IO.Temp
import System.FilePath

-- | The location of GHC and arguments to pass it.
data Compiler = Compiler { compPath :: FilePath
                         , compArgs :: [String]
                         }
    deriving (Show)

-- | A GHC compiler with some typical flags.
basicCompiler :: FilePath -> Compiler
basicCompiler ghcPath =
    Compiler { compPath = ghcPath
             , compArgs = ["-dcmm-lint", "-dasm-lint", "-O0"]
             }

addArgs :: Compiler -> [String] -> Compiler
addArgs c args = c { compArgs = compArgs c ++ args }

-- | Compile a set of compilation units.
compile :: Compiler
        -> FilePath   -- ^ working directory
        -> [FilePath] -- ^ sources
        -> FilePath   -- ^ output path
        -> [String]   -- ^ other arguments
        -> IO ()
compile comp workDir srcs out args = do
    runProcess' $ inTmp (proc (compPath comp) allArgs)
  where
    allArgs = compArgs comp ++ srcs ++ args ++ ["-o", out]
    inTmp c = c { cwd = Just workDir }
    runProcess' p = do
        (_, _, _, hdl) <- createProcess p
        ExitSuccess <- waitForProcess hdl
        return ()

compileHs :: Compiler -> String -> IO TestProgram
compileHs = compileOne "hs"

compileCmm :: Compiler -> String -> IO TestProgram
compileCmm = compileOne "cmm"

compileC :: Compiler -> String -> IO TestProgram
compileC = compileOne "c"

compileOne :: String -> Compiler -> String -> IO TestProgram
compileOne ext comp contents = withTempDirectory "." "tmp" $ \tmpDir -> do
    writeFile (tmpDir </> srcName) contents
    compile comp tmpDir [srcName] objName ["-c"]
    obj <- BS.readFile (tmpDir </> objName)
    return (TestProgram [obj])
  where
    srcName = "test" <.> ext
    objName = "out.o"

-- | A set of object files which comprise a test program.
newtype TestProgram = TestProgram { _objects :: [BS.ByteString] }
    deriving (Semigroup)

writeObjectsIn :: FilePath -> TestProgram -> IO [FilePath]
writeObjectsIn dir (TestProgram objs) = do
    let writeObj :: Integer -> BS.ByteString -> IO FilePath
        writeObj i obj = do
            let fname = "obj"++show i++".o"
            BS.writeFile (dir </> fname) obj
            return fname
    sequence $ zipWith writeObj [0..] objs

