module System.ByteOrder.Class
  ( Bytes(..)
  ) where

import GHC.ByteOrder (ByteOrder(..),targetByteOrder)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Word (byteSwap16,byteSwap32,byteSwap64)

class Bytes a where
  toBigEndian :: a -> a
  toLittleEndian :: a -> a

instance Bytes Word8 where
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
