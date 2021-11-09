-- | Fixed-width numbers. In hindsight this should have been called
-- "bit-pattern".
module Number
    ( Signedness(..)
    , signednessTag
      -- * Fixed-width bit patterns
    , Number
    , numberWidth
    , toSigned
    , toUnsigned
    , fromSigned
    , fromUnsigned
    , fromUnsignedC
    , chooseNumber
    , n8, n16, n32, n64
    , ones
    , truncateNumber
    , signExtNumber
    , zeroExtNumber
    , shiftRa, shiftRl
    , divNumber, remNumber
      -- * Bit patterns of any width
    , SomeNumber(..)
    , someNumberWidth
    , someNumberToUnsigned
    ) where

import GHC.Stack
import Data.Bits
import Data.Proxy
import Numeric (showHex)
import Numeric.Natural
import Test.QuickCheck hiding ((.&.))
import Prelude hiding (truncate)

import Width

data Signedness = Signed | Unsigned
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

signednessTag :: Signedness -> String
signednessTag Signed   = "s"
signednessTag Unsigned = "u"

instance Arbitrary Signedness where
    arbitrary = elements [Signed, Unsigned]

newtype Number (width :: Width) where
    Number :: Natural -> Number width
  deriving (Eq, Ord)

instance Show (Number width) where
    showsPrec _ (Number n)
      | n < 10    = shows n
      | otherwise = showString "0x" . showHex n

numberWidth :: forall width. (KnownWidth width)
            => Number width -> Width
numberWidth _ = knownWidth @width

data SomeNumber where
    SomeNumber :: forall w. (KnownWidth w) => Number w -> SomeNumber

someNumberWidth :: SomeNumber -> Width
someNumberWidth (SomeNumber n) = numberWidth n

someNumberToUnsigned :: SomeNumber -> Natural
someNumberToUnsigned (SomeNumber n) = toUnsigned n

instance Show SomeNumber where
    show (SomeNumber (n :: Number w)) =
        "SomeNumber @" ++ show (knownWidth @w) ++ " " ++ show n

instance Arbitrary SomeNumber where
    arbitrary =
        oneof $ forAllWidths $ \(_ :: Proxy w) -> SomeNumber @w <$> arbitrary

-- | Interpret a bit pattern as an unsigned number.
toUnsigned :: Number width -> Natural
toUnsigned (Number n) = n

-- | Interpret a bit pattern as a signed number.
toSigned
    :: forall width. (KnownWidth width)
    => Number width -> Integer
toSigned n
  | n `testBit` signBit = negate $ toInteger $ toUnsigned $ twosComplement n
  | otherwise           = toInteger $ toUnsigned n
  where
    signBit = widthBits (knownWidth @width) - 1

fromUnsigned
    :: forall width. (KnownWidth width)
    => Natural -> Number width
fromUnsigned n = Number $ truncate (knownWidth @width) n

fromSigned
    :: forall width. (HasCallStack, KnownWidth width)
    => Integer -> Number width
fromSigned n
  | n < negate b
              = error "fromSigned: underflow"
  | n > b     = error "fromSigned: overflow"
  | n < 0     = Number $ fromIntegral $ (1 `shiftL` w) + n
  | otherwise = Number $ fromIntegral n
  where
    w = widthBits (knownWidth @width)
    b = 2^(w-1) - 1

-- | Sample a 'Number'.
chooseNumber
    :: (KnownWidth width)
    => (Number width, Number width) -> Gen (Number width)
chooseNumber (Number a, Number b) =
    fromUnsigned . fromIntegral <$> chooseInteger (fromIntegral a, fromIntegral b)


-- | Checked.
fromUnsignedC
    :: forall width. (KnownWidth width)
    => Natural -> Number width
fromUnsignedC n
  | n == n'   = Number n'
  | otherwise = error "fromUnsignedC: out of range"
  where
    n' = truncate (knownWidth @width) n

n8 :: Natural -> Number W8
n8 = fromUnsigned

n16 :: Natural -> Number W16
n16 = fromUnsigned

n32 :: Natural -> Number W32
n32 = fromUnsigned

n64 :: Natural -> Number W64
n64 = fromUnsigned

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
    arbitrary = chooseNumber (minBound, maxBound)
    shrink (Number 0) = []
    shrink (Number 1) = [fromUnsigned 0]
    shrink (Number x) = map fromUnsigned $ (0:) $ reverse $ takeWhile (/= 0) $ tail $ iterate (`div` 2) x

instance (KnownWidth width) => Num (Number width) where
    (+) = liftBinOp (+)
    (*) = liftBinOp (*)
    negate = twosComplement
    signum _ = 1
    abs = id
    fromInteger = fromUnsigned . fromIntegral

instance (KnownWidth width) => Real (Number width) where
    toRational (Number n) = toRational n

instance (KnownWidth width) => Bits (Number width) where
    (.&.) = liftBinOp (.&.)
    (.|.) = liftBinOp (.|.)
    xor   = liftBinOp xor
    complement n = ones `xor` n
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
    bit i = fromUnsigned $ bit i
    popCount (Number n) = popCount n

ones :: forall width. (KnownWidth width) => Number width
ones = (1 `shiftL` widthBits (knownWidth @width)) - 1

twosComplement
    :: forall width. KnownWidth width
    => Number width -> Number width
twosComplement (Number n) =
    fromUnsigned ((1 `shiftL` w) - n)
  where w = widthBits (knownWidth @width)

liftUnOp
    :: forall width. KnownWidth width
    => (Natural -> Natural)
    -> Number width -> Number width
liftUnOp f (Number a) =
    fromUnsigned (f a)

liftBinOp
    :: forall width. KnownWidth width
    => (Natural -> Natural -> Natural)
    -> Number width -> Number width -> Number width
liftBinOp f (Number a) (Number b) = fromUnsigned (f a b)

truncateNumber
    :: forall wide narrow. (KnownWidth narrow)
    => Number wide -> Number narrow
truncateNumber (Number n) = fromUnsigned n

signExtend :: Int   -- ^ Initial width
           -> Int   -- ^ Target width
           -> Natural -> Natural
signExtend w0 w1 n
  | n `testBit` signBit = n .|. (highBits `shiftL` w0)
  | otherwise           = n
  where
    signBit = w0 - 1
    highBits = (1 `shiftL` (w1 - w0)) - 1

signExtNumber
    :: forall wide narrow. (KnownWidth narrow, KnownWidth wide)
    => Number narrow -> Number wide
signExtNumber (Number n) = Number $ signExtend narrowW wideW n
  where
    narrowW  = widthBits $ knownWidth @narrow
    wideW    = widthBits $ knownWidth @wide

zeroExtNumber
    :: forall wide narrow. (KnownWidth wide)
    => Number narrow -> Number wide
zeroExtNumber (Number n) = fromUnsigned n

shiftRa
    :: forall width. (KnownWidth width)
    => Number width -> Int -> Number width
shiftRa (Number n) s
  | s < 0     = error "negative shift"
  | otherwise = Number $ signExtend (w-s) w (shiftR n s)
  where
    w = widthBits $ knownWidth @width

shiftRl
    :: forall width. (KnownWidth width)
    => Number width -> Int -> Number width
shiftRl n s
  | s < 0     = error "negative shift"
  | otherwise = shiftR n s

divU, divS, remU, remS
    :: forall width. (KnownWidth width)
    => Number width -> Number width -> Number width
divU a b = fromUnsignedC $ toUnsigned a `quot` toUnsigned b
divS a b = fromSigned $ toSigned a `quot` toSigned b
remU a b = fromUnsignedC $ toUnsigned a `rem` toUnsigned b
remS a b = fromSigned $ toSigned a `rem` toSigned b

divNumber
    :: forall width. (KnownWidth width)
    => Signedness -> Number width -> Number width -> Number width
divNumber Signed   = divS
divNumber Unsigned = divU

remNumber
    :: forall width. (KnownWidth width)
    => Signedness -> Number width -> Number width -> Number width
remNumber Signed   = remS
remNumber Unsigned = remU
