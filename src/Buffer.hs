module Buffer
    ( buffer
    , bufferSize
    , validOffset
    ) where

import Numeric.Natural
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

bufferSize :: Natural
bufferSize = 1 `shiftL` 22

buffer :: BS.ByteString
buffer = BS.pack $ take (fromIntegral bufferSize) contents
  where
    contents :: [Word8]
    contents = replicate 8 0 ++ [ fromIntegral i | i <- [(0 :: Int) ..] ]

validOffset :: Natural -> Bool
validOffset off = off < bufferSize

