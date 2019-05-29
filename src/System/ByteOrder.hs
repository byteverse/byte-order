{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
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

Suppose there is a protocol for aggregating numbers that uses stream
sockets for communication. The protocol interprets all numbers as
unsigned. It is described as follows:

1. The client sends the server a little-endian 16-bit number @N@.
   This is the count of numbers that will follow.
2. The client sends the server @N@ little-endian 64-bit numbers.
3. The server responds with two little-endian 64-bit numbers:
   the sum and the product.

Assume the existence of a @send@ and @receive@ that block until
the total number of requested bytes have been handled. Additionally,
assume a @typed@ and @untyped@ function that convert between
'PrimArray' and 'ByteArray' by changing out the data constructor.

> send :: Socket -> ByteArray -> IO ()
> receive :: Socket -> Int -> IO ByteArray
> typed :: ByteArray -> PrimArray a
> untyped :: PrimArray a -> ByteArray

For simplicity, all error-handling is omitted. With the type-directed
interface, the server is implemented as:

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

Not all of the explicit type annotations are needed, but they have been
provided for additional clarity. As long as the user ensures that the
typed primitive arrays use 'Fixed' in their element types, the endianness
conversions are guaranteed to be correct.

-}
module System.ByteOrder
  ( -- * Types
    ByteOrder(..)
  , Fixed(..)
    -- * System Byte Order
  , targetByteOrder
    -- * Classes
  , Bytes
  , FixedOrdering
    -- * Convert
  , toBigEndian
  , toLittleEndian
  , fromBigEndian
  , fromLittleEndian
  ) where

import Data.Kind (Type)
import Data.Primitive.Types (Prim)
import GHC.ByteOrder (ByteOrder(..),targetByteOrder)
import System.ByteOrder.Class (Bytes(..),FixedOrdering,toFixedEndian)

import qualified Data.Primitive.Types as PM

-- | Convert from a big-endian word to a native-endian word.
fromBigEndian :: Bytes a => a -> a
fromBigEndian = toBigEndian

-- | Convert from a little-endian word to a native-endian word.
fromLittleEndian :: Bytes a => a -> a
fromLittleEndian = toLittleEndian

instance (FixedOrdering b, Prim a, Bytes a) => Prim (Fixed b a) where
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

-- | A word whose byte order is specified (not platform dependent)
-- when working with 'Prim' and 'Storable'.
newtype Fixed :: ByteOrder -> Type -> Type where
  Fixed :: { getFixed :: a } -> Fixed b a

type role Fixed phantom representational
