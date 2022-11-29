{-# LANGUAGE CPP #-}

#include "MachDeps.h"

-- | Bit widths
module Width
    ( -- * Width
      Width(..)
    , WordSize
    , wordSize
    , widthBits
    , KnownWidth
    , knownWidth
    , forAllWidths
    , allWidths
    , SomeWidth(..)
    , truncate
      -- * Comparing width
    , WidthOrdering(..)
    , WiderThan
    , compareWidths
      -- * Bounds
    , unsignedBounds
    , signedBounds
    ) where

import Data.Bits as Bits
import Unsafe.Coerce
import Data.Proxy
import Test.QuickCheck hiding ((.&.))
import Prelude hiding (truncate)

#if defined(WORD_SIZE_32BIT)
type WordSize = W32
#elif defined(WORD_SIZE_64BIT)
type WordSize = W64
#else
#error unknown word size
#endif

wordSize :: Width
wordSize = knownWidth @WordSize

data Width = W8 | W16 | W32 | W64
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Arbitrary Width where
    arbitrary = arbitraryBoundedEnum

widthBits :: Width -> Int
widthBits W8  = 8
widthBits W16 = 16
widthBits W32 = 32
widthBits W64 = 64

class KnownWidth (width :: Width) where
    knownWidth :: Width

instance KnownWidth W8  where
    knownWidth = W8
instance KnownWidth W16 where
    knownWidth = W16
instance KnownWidth W32 where
    knownWidth = W32
instance KnownWidth W64 where
    knownWidth = W64

class WiderThan wide narrow

instance WiderThan W16 W8
instance WiderThan W32 W8
instance WiderThan W32 W16
instance WiderThan W64 W8
instance WiderThan W64 W16
instance WiderThan W64 W32

data WidthOrdering a b where
  Narrower  :: (b `WiderThan` a) => WidthOrdering a b
  SameWidth :: (b ~ a)           => WidthOrdering a b
  Wider     :: (a `WiderThan` b) => WidthOrdering a b

compareWidths
    :: forall a b proxy. (KnownWidth a, KnownWidth b)
    => proxy a -> proxy b
    -> WidthOrdering a b
compareWidths _ _
  | a > b     = unsafeCoerce $ Wider @W16 @W8
  | a < b     = unsafeCoerce $ Narrower @W16 @W8
  | otherwise = unsafeCoerce $ SameWidth @W8
  where
    a = knownWidth @a
    b = knownWidth @b

truncate :: (Num a, Bits a) => Width -> a -> a
truncate w n =
    n .&. ((1 `shiftL` widthBits w) - 1)

data SomeWidth where
    SomeWidth :: forall width. (KnownWidth width) => Proxy width -> SomeWidth

allWidths :: [SomeWidth]
allWidths =
    [ SomeWidth (Proxy @W8)
    , SomeWidth (Proxy @W16)
    , SomeWidth (Proxy @W32)
#if WORD_SIZE_IN_BITS == 64
    , SomeWidth (Proxy @W64)
#endif
    ]

forAllWidths :: (forall w. (KnownWidth w) => Proxy w -> r) -> [r]
forAllWidths f =
    [ f proxy
    | SomeWidth proxy <- allWidths
    ]

-- | Minimum and maximum bounds (inclusive) of the given width when used to
-- encode a signed integer via twos-complement.
unsignedBounds :: Width -> (Integer, Integer)
unsignedBounds w = (0, 2^(widthBits w) - 1)

-- | Minimum and maximum bounds (inclusive) of the given width when used to
-- encode a signed integer via twos-complement.
signedBounds :: Width -> (Integer, Integer)
signedBounds w = (negate $ 2^(widthBits w - 1), 2^(widthBits w - 1) - 1)
