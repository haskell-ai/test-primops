-- | Bit widths
module Width where

import Data.Bits as Bits
import Test.QuickCheck hiding ((.&.))
import Prelude hiding (truncate)

data Width = W8 | W16 | W32 | W64
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Width where
    arbitrary = arbitraryBoundedEnum

widthBits :: Width -> Int
widthBits W8  = 8
widthBits W16 = 16
widthBits W32 = 32
widthBits W64 = 64

class KnownWidth (w :: Width) where
    knownWidth :: Width

instance KnownWidth W8 where knownWidth = W8
instance KnownWidth W16 where knownWidth = W16
instance KnownWidth W32 where knownWidth = W32
instance KnownWidth W64 where knownWidth = W64

class Wider wide narrow

instance Wider W16 W8
instance Wider W32 W8
instance Wider W64 W8
instance Wider W32 W16
instance Wider W64 W16
instance Wider W64 W32

truncate :: (Num a, Bits a) => Width -> a -> a
truncate w n =
    n .&. ((1 `shiftL` widthBits w) - 1)

