{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

{-| This module offers an interface to portably work with byte
arrays whose contents are known to be of a fixed endianness.
There are two ways to use this module:

* Untyped Conversions: The functions 'toBigEndian', 'toLittleEndian',
  'fromBigEndian', and 'fromLittleEndian' convert between native-endian
  words and big/little-endian words. The word resulting from
  @to(Big|Little)Endian@ should be written to a primitive byte
  array or a pointer afterwards. (There is no other purpose of
  such a conversion.) Similarly, the argument to @from(Big|Little)Endian@
  should be a word that was read from a primitive byte array or
  a pointer. This interface is useful when serializing or deserializing
  a data structure with fields of varying sizes.
* Typed Conversions: The type 'Fixed' provides a convenient
  type-directed interface to working with arrays of homogenous words.
  This interface is easier to use and should be preferred when
  possible.

The example at the bottom of this page demonstrates how to use the
type-directed interface.
-}

module System.ByteOrder
  ( -- * Types
    ByteOrder(..)
  , Fixed(..)
    -- * Classes
  , Bytes
  , FixedOrdering
    -- * Conversion
  , toBigEndian
  , toLittleEndian
  , fromBigEndian
  , fromLittleEndian
    -- * System Byte Order
  , targetByteOrder
    -- * Example
    -- $example
  ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Kind (Type)
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned)
import Data.Primitive.Types (Prim)
import Foreign.Ptr (Ptr,castPtr)
import Foreign.Storable (Storable)
import GHC.ByteOrder (ByteOrder(..),targetByteOrder)
import System.ByteOrder.Class (Bytes(..),FixedOrdering,toFixedEndian)

import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.ByteArray.Unaligned as PMU
import qualified Foreign.Storable as FS

-- | Convert from a big-endian word to a native-endian word.
fromBigEndian :: Bytes a => a -> a
fromBigEndian = toBigEndian

-- | Convert from a little-endian word to a native-endian word.
fromLittleEndian :: Bytes a => a -> a
fromLittleEndian = toLittleEndian

-- | A word whose byte order is specified (not platform dependent)
-- when working with 'Prim', 'Storable', and @PrimUnaligned@ (this
-- last instance is provided alongside the typeclass itself in the
-- @primitive-unaligned@ library).
newtype Fixed :: ByteOrder -> Type -> Type where
  Fixed :: forall (b :: ByteOrder) (a :: Type). { getFixed :: a } -> Fixed b a

type role Fixed nominal representational

deriving newtype instance Enum a => Enum (Fixed b a)
deriving newtype instance Eq a => Eq (Fixed b a)
deriving newtype instance Integral a => Integral (Fixed b a)
deriving newtype instance Num a => Num (Fixed b a)
deriving newtype instance Ord a => Ord (Fixed b a)
deriving newtype instance Real a => Real (Fixed b a)

instance (FixedOrdering b, Bits a, Bytes a) => Bits (Fixed b a) where
  {-# inline (.&.) #-}
  {-# inline (.|.) #-}
  {-# inline xor #-}
  {-# inline complement #-}
  {-# inline shift #-}
  {-# inline rotate #-}
  {-# inline zeroBits #-}
  {-# inline bit #-}
  {-# inline setBit #-}
  {-# inline clearBit #-}
  {-# inline complementBit #-}
  {-# inline testBit #-}
  {-# inline bitSizeMaybe #-}
  --{-# inline bitSize #-}
  {-# inline isSigned #-}
  {-# inline shiftL #-}
  {-# inline unsafeShiftL #-}
  {-# inline shiftR #-}
  {-# inline unsafeShiftR #-}
  {-# inline rotateL #-}
  {-# inline rotateR #-}
  {-# inline popCount #-}
  Fixed x .&. Fixed y = Fixed (toFixedEndian @b x .&. toFixedEndian @b y)
  Fixed x .|. Fixed y = Fixed (toFixedEndian @b x .|. toFixedEndian @b y)
  Fixed x `xor` Fixed y = Fixed (toFixedEndian @b x `xor` toFixedEndian @b y)
  complement (Fixed x) = Fixed (complement (toFixedEndian @b x))
  shift (Fixed x) i = Fixed (shift (toFixedEndian @b x) i)
  rotate (Fixed x) i = Fixed (rotate (toFixedEndian @b x) i)
  zeroBits = Fixed (toFixedEndian @b zeroBits)
  bit i = Fixed (toFixedEndian @b (bit i))
  setBit (Fixed x) i = Fixed (setBit (toFixedEndian @b x) i)
  clearBit (Fixed x) i = Fixed (clearBit (toFixedEndian @b x) i)
  complementBit (Fixed x) i = Fixed (complementBit (toFixedEndian @b x) i)
  testBit (Fixed x) i = testBit (toFixedEndian @b x) i
  bitSizeMaybe (Fixed x) = bitSizeMaybe (toFixedEndian @b x)
  bitSize (Fixed x) = bitSize (toFixedEndian @b x)
  isSigned (Fixed x) = isSigned (toFixedEndian @b x)
  shiftL (Fixed x) i = Fixed (shiftL (toFixedEndian @b x) i)
  unsafeShiftL (Fixed x) i = Fixed (unsafeShiftL (toFixedEndian @b x) i)
  shiftR (Fixed x) i = Fixed (shiftR (toFixedEndian @b x) i)
  unsafeShiftR (Fixed x) i = Fixed (unsafeShiftR (toFixedEndian @b x) i)
  rotateL (Fixed x) i = Fixed (rotateL (toFixedEndian @b x) i)
  rotateR (Fixed x) i = Fixed (rotateR (toFixedEndian @b x) i)
  popCount (Fixed x) = popCount (toFixedEndian @b x)

instance (FixedOrdering b, FiniteBits a, Bytes a) => FiniteBits (Fixed b a) where
  {-# inline finiteBitSize #-}
  {-# inline countLeadingZeros #-}
  {-# inline countTrailingZeros #-}
  finiteBitSize (Fixed x) = finiteBitSize (toFixedEndian @b x)
  countLeadingZeros (Fixed x) = countLeadingZeros (toFixedEndian @b x)
  countTrailingZeros (Fixed x) = countTrailingZeros (toFixedEndian @b x)

instance (FixedOrdering b, Prim a, Bytes a) => Prim (Fixed b a) where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = PM.sizeOf# (undefined :: a)
  alignment# _ = PM.alignment# (undefined :: a)
  indexByteArray# a i = Fixed (toFixedEndian @b (PM.indexByteArray# a i))
  readByteArray# a i s0 = case PM.readByteArray# a i s0 of
    (# s1, x #) -> (# s1, Fixed (toFixedEndian @b x) #)
  writeByteArray# a i (Fixed x) = PM.writeByteArray# a i (toFixedEndian @b x)
  setByteArray# a i n (Fixed x) = PM.setByteArray# a i n (toFixedEndian @b x)
  indexOffAddr# a i = Fixed (toFixedEndian @b (PM.indexOffAddr# a i))
  readOffAddr# a i s0 = case PM.readOffAddr# a i s0 of
    (# s1, x #) -> (# s1, Fixed (toFixedEndian @b x) #)
  writeOffAddr# a i (Fixed x) = PM.writeOffAddr# a i (toFixedEndian @b x)
  setOffAddr# a i n (Fixed x) = PM.setOffAddr# a i n (toFixedEndian @b x)

instance (FixedOrdering b, PrimUnaligned a, Bytes a) => PrimUnaligned (Fixed b a) where
  {-# inline indexUnalignedByteArray# #-}
  {-# inline readUnalignedByteArray# #-}
  {-# inline writeUnalignedByteArray# #-}
  indexUnalignedByteArray# a i = Fixed (toFixedEndian @b (PMU.indexUnalignedByteArray# a i))
  readUnalignedByteArray# a i s0 = case PMU.readUnalignedByteArray# a i s0 of
    (# s1, x #) -> (# s1, Fixed (toFixedEndian @b x) #)
  writeUnalignedByteArray# a i (Fixed x) = PMU.writeUnalignedByteArray# a i (toFixedEndian @b x)

instance (FixedOrdering b, Storable a, Bytes a) => Storable (Fixed b a) where
  {-# inline sizeOf #-}
  {-# inline alignment #-}
  {-# inline peekElemOff #-}
  {-# inline pokeElemOff #-}
  {-# inline peekByteOff #-}
  {-# inline pokeByteOff #-}
  {-# inline peek #-}
  {-# inline poke #-}
  sizeOf _ = FS.sizeOf (undefined :: a)
  alignment _ = FS.alignment (undefined :: a)
  peekElemOff p i = fmap (Fixed . toFixedEndian @b) (FS.peekElemOff (fromFixedPtr p) i)
  pokeElemOff p i (Fixed x) = FS.pokeElemOff (fromFixedPtr p) i (toFixedEndian @b x)
  peekByteOff p i = fmap (Fixed . toFixedEndian @b) (FS.peekByteOff p i)
  pokeByteOff p i (Fixed x) = FS.pokeByteOff p i (toFixedEndian @b x)
  peek p = fmap (Fixed . toFixedEndian @b) (FS.peek (fromFixedPtr p))
  poke p (Fixed x) = FS.poke (fromFixedPtr p) (toFixedEndian @b x)

fromFixedPtr :: Ptr (Fixed b a) -> Ptr a
{-# inline fromFixedPtr #-}
fromFixedPtr = castPtr

{- $example
Suppose there is a protocol for aggregating numbers that uses stream
sockets for communication. The protocol interprets all numbers as
unsigned. It is described as follows:

1. The client sends the server a little-endian 16-bit number @N@.
   This is how many numbers will follow.
2. The client sends @N@ little-endian 64-bit numbers to the server.
3. The server responds with two little-endian 64-bit numbers:
   the sum and the product of the @N@ numbers it received.

Assume the existence of a @send@ and @receive@ that block until
the total number of requested bytes have been handled. They both
work on their argument arrays starting at index zero, which ensures
that any 2-byte, 4-byte, or 8-byte types will be aligned properly.
(GHC always machine-word aligns the payload of a byte array.)
Additionally, assume the @typed@ and @untyped@ functions that convert between
'PrimArray' and 'ByteArray' by changing out the data constructor.

> send :: Socket -> ByteArray -> IO ()
> receive :: Socket -> Int -> IO ByteArray
> typed :: ByteArray -> PrimArray a
> untyped :: PrimArray a -> ByteArray

For simplicity, all error-handling is omitted. With the type-directed
interface, the server is implemented as:

> import Data.Primitive.ByteArray
> import Data.Primitive.PrimArray
> import System.ByteOrder
>
> server :: Socket -> IO a
> server sckt = forever $ do
>   totalByteArray <- receive sckt 2
>   let totalPrimArray = typed totalByteArray :: PrimArray (Fixed 'LittleEndian Word16)
>   let Fixed total = indexPrimArray totalPrimArray 0
>   numberByteArray <- receive sckt (8 * fromIntegral @Word16 @Int total)
>   let (sum,prod) = foldlPrimArray'
>         (\(!sumN,!prodN) (Fixed n) -> (sumN + n, prodN * n))
>         (0,1)
>         (typed numberByteArray :: PrimArray (Fixed 'LittleEndian Word64))
>   reply :: MutablePrimArray RealWorld (Fixed 'LittleEndian Word64) <- newPrimArray 2
>   writePrimArray reply 0 (Fixed sum)
>   writePrimArray reply 1 (Fixed prod)
>   send sckt . untyped =<< unsafeFreezePrimArray reply

Not every explicit type annotation above is needed. Some are provided
for the reader's benefit. As long as the user ensures that the
typed primitive arrays use 'Fixed' in their element types, the endianness
conversions are guaranteed to be correct.

-}
