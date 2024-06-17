module Buffer
    ( buffer
    , bufferSize
    , validOffset
    ) where

import Numeric.Natural
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

import Width

bufferSize :: Natural
bufferSize = 1 `shiftL` 22

buffer :: BS.ByteString
buffer = BS.pack $ take (fromIntegral bufferSize) contents
  where
    contents :: [Word8]
    contents = replicate 8 0 ++ [ fromIntegral i | i <- [(0 :: Int) ..] ]

-- | Can we read a `Width` wide value from the given offset?
--
-- Width is important as a wide read might try to read past the buffer
-- even if the offset points at an address within the buffer.
validOffset :: Natural -> Width -> Bool
validOffset off w =
  off + (fromIntegral $ widthBytes w) <= bufferSize
