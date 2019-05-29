{-# language TypeApplications #-}

module System.ByteOrder.Class
  ( Bytes(..)
  ) where

import GHC.ByteOrder (ByteOrder(..),targetByteOrder)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Word (byteSwap16,byteSwap32,byteSwap64)
import Data.Int (Int8,Int16,Int32,Int64)

-- | Types that are represented as a fixed-sized word. For these
-- types, the bytes can be swapped. The instances of this class
-- use byteswapping primitives and compile-time knowledge of native
-- endianness to provide portable endianness conversion functions.
class Bytes a where
  -- | Convert from a native-endian word to a big-endian word.
  toBigEndian :: a -> a
  -- | Convert from a native-endian word to a little-endian word.
  toLittleEndian :: a -> a

instance Bytes Word8 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = id
  toLittleEndian = id

instance Bytes Word16 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap16
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap16
    LittleEndian -> id

instance Bytes Word32 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap32
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap32
    LittleEndian -> id

instance Bytes Word64 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap64
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap64
    LittleEndian -> id

instance Bytes Int8 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = id
  toLittleEndian = id

instance Bytes Int16 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
        fromIntegral @Word16 @Int16
      . byteSwap16
      . fromIntegral @Int16 @Word16
  toLittleEndian = case targetByteOrder of
    BigEndian ->
        fromIntegral @Word16 @Int16
      . byteSwap16
      . fromIntegral @Int16 @Word16
    LittleEndian -> id

instance Bytes Int32 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
        fromIntegral @Word32 @Int32
      . byteSwap32
      . fromIntegral @Int32 @Word32
  toLittleEndian = case targetByteOrder of
    BigEndian ->
        fromIntegral @Word32 @Int32
      . byteSwap32
      . fromIntegral @Int32 @Word32
    LittleEndian -> id

instance Bytes Int64 where
  {-# inline toBigEndian #-}
  {-# inline toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
        fromIntegral @Word64 @Int64
      . byteSwap64
      . fromIntegral @Int64 @Word64
  toLittleEndian = case targetByteOrder of
    BigEndian ->
        fromIntegral @Word64 @Int64
      . byteSwap64
      . fromIntegral @Int64 @Word64
    LittleEndian -> id
