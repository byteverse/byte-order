module System.ByteOrder
  ( -- * Types
    ByteOrder(..)
    -- * System Byte Order
  , targetByteOrder
    -- * Class
  , Bytes
    -- * Convert
  , toBigEndian
  , toLittleEndian
  , fromBigEndian
  , fromLittleEndian
  ) where

import GHC.ByteOrder (ByteOrder(..),targetByteOrder)
import System.ByteOrder.Class (Bytes(..))

fromBigEndian :: Bytes a => a -> a
fromBigEndian = toBigEndian

fromLittleEndian :: Bytes a => a -> a
fromLittleEndian = toLittleEndian
