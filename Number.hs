-- | Fixed-width numbers.
module Number
    ( Number
    , getNumber
    , chooseNumber
    , mkNumber
    , n8, n16, n32, n64
    , truncateNumber
    , signExtNumber
    , zeroExtNumber
    ) where

import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Prelude hiding (truncate)

import Width

newtype Number (width :: Width) where
    Number :: Integer -> Number width
  deriving (Eq, Ord)

instance Show (Number width) where
    show (Number n) = show n

getNumber :: Number width -> Integer
getNumber (Number n) = n

chooseNumber :: (KnownWidth width)
             => (Number width, Number width) -> Gen (Number width)
chooseNumber (Number a, Number b) =
    mkNumber <$> chooseInteger (a, b)

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
    arbitrary = mkNumber <$> chooseInteger (a,b)
      where
        Number a = minBound @(Number width)
        Number b = maxBound @(Number width)
    shrink (Number 0) = []
    shrink (Number 1) = [mkNumber 0]
    shrink (Number x) = map mkNumber [0, 1, x `div` 2]

instance (KnownWidth width) => Num (Number width) where
    (+) = liftBinOp (+)
    (-) = liftBinOp (-)
    (*) = liftBinOp (*)
    signum _ = 1
    abs = id
    fromInteger = mkNumber

instance (KnownWidth width) => Bits (Number width) where
    (.&.) = liftBinOp (.&.)
    (.|.) = liftBinOp (.|.)
    xor   = liftBinOp xor
    complement = liftUnOp complement
    n `shift` s
      | s < negate w = 0
      | s > w        = 0
      | otherwise    = liftUnOp (`shift` s) n
      where w = widthBits (knownWidth @width)
    rotate = undefined -- TODO
    bitSize _ = widthBits (knownWidth @width)
    bitSizeMaybe = Just . bitSize
    isSigned _ = False
    testBit (Number n) i = n `testBit` i
    bit i = mkNumber $ bit i
    popCount (Number n) = popCount n

liftUnOp
    :: forall width. KnownWidth width
    => (Integer -> Integer)
    -> Number width -> Number width
liftUnOp f (Number a) =
    mkNumber (f a)

liftBinOp
    :: forall width. KnownWidth width
    => (Integer -> Integer -> Integer)
    -> Number width -> Number width -> Number width
liftBinOp f (Number a) (Number b) = mkNumber (f a b)

truncateNumber
    :: forall wide narrow. (KnownWidth narrow)
    => Number wide -> Number narrow
truncateNumber (Number n) = mkNumber n

signExtNumber
    :: forall wide narrow. (KnownWidth narrow, KnownWidth wide)
    => Number narrow -> Number wide
signExtNumber (Number n')
  | n' `testBit` signBit = Number $ n' .|. highBits
  | otherwise            = Number n'
  where
    highBits = ((1 `shiftL` (wideW - narrowW)) - 1) `shiftL` narrowW
    narrowW  = widthBits $ knownWidth @narrow
    wideW    = widthBits $ knownWidth @wide
    signBit  = narrowW - 1

zeroExtNumber
    :: forall wide narrow. (KnownWidth wide)
    => Number narrow -> Number wide
zeroExtNumber (Number n) = mkNumber n
