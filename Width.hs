-- | Bit widths
module Width where

import Data.Bits as Bits
import Data.Proxy
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

class KnownWidth (width :: Width) where
    knownWidth :: Width
    narrowings :: forall a.
                  (forall narrow. (KnownWidth narrow, width `WiderThan` narrow) => Proxy narrow -> a)
               -> [a]
    extensions :: forall a.
                  (forall wide. (KnownWidth wide, wide `WiderThan` width) => Proxy wide -> a)
               -> [a]

instance KnownWidth W8  where
    knownWidth = W8
    narrowings _ = []
    extensions f = [f @W16 Proxy, f @W32 Proxy, f @W64 Proxy]
instance KnownWidth W16 where
    knownWidth = W16
    narrowings f = [f @W8 Proxy]
    extensions f = [f @W32 Proxy, f @W64 Proxy]
instance KnownWidth W32 where
    knownWidth = W32
    narrowings f = [f @W8 Proxy, f @W16 Proxy]
    extensions f = [f @W64 Proxy]
instance KnownWidth W64 where
    knownWidth = W64
    narrowings f = [f @W8 Proxy, f @W16 Proxy, f @W32 Proxy]
    extensions f = []

class WiderThan wide narrow

instance WiderThan W16 W8
instance WiderThan W32 W8
instance WiderThan W32 W16
instance WiderThan W64 W8
instance WiderThan W64 W16
instance WiderThan W64 W32

truncate :: (Num a, Bits a) => Width -> a -> a
truncate w n =
    n .&. ((1 `shiftL` widthBits w) - 1)

