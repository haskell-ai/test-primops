-- | Fixed-width numbers.
module Number where

import Data.Bits as Bits
import Test.QuickCheck hiding ((.&.))
import Prelude hiding (truncate)

import Width

data Number (width :: Width) where
    Number :: Integer -> Number width
  deriving (Eq, Ord)

instance Show (Number width) where
    show (Number n) = show n

mkNumber :: forall width. (KnownWidth width)
         => Integer -> Number width
mkNumber n = Number $ truncate (knownWidth @width) n

n8 :: Integer -> Number W8
n8 = mkNumber

n16 :: Integer -> Number W16
n16 = mkNumber

n32 :: Integer -> Number W32
n32 = mkNumber

n64 :: Integer -> Number W64
n64 = mkNumber

liftBinOp
    :: forall width. KnownWidth width
    => (Integer -> Integer -> Integer)
    -> Number width -> Number width -> Number width
liftBinOp f (Number a) (Number b) =
    Number $ truncate (knownWidth @width) (f a b)

instance (KnownWidth width) => Enum (Number width) where
    fromEnum (Number n) = fromIntegral n
    toEnum (Number . fromIntegral -> n)
      | n < minBound = error "underflow"
      | n > maxBound = error "overflow"
      | otherwise    = n

instance (KnownWidth width) => Bounded (Number width) where
    minBound = Number 0
    maxBound = Number ((1 `shiftL` widthBits (knownWidth @width)) - 1)

instance (KnownWidth width) => Arbitrary (Number width) where
    arbitrary = arbitraryBoundedEnum

truncateNumber :: forall wide narrow. (KnownWidth narrow)
               => Number wide -> Number narrow
truncateNumber (Number n) =
    Number (truncate (knownWidth @narrow) n)

negateNumber :: forall wide narrow. (KnownWidth narrow)
             => Number wide -> Number narrow
negateNumber (Number n) =
    Number $ truncate (knownWidth @narrow) (negate n)

signExtNumber :: forall wide narrow. (KnownWidth narrow)
              => Number wide -> Number narrow
signExtNumber (Number n) = undefined

zeroExtNumber :: forall wide narrow. (KnownWidth narrow)
              => Number wide -> Number narrow
zeroExtNumber (Number n) = Number n
