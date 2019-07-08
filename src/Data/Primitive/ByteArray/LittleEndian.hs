module Data.Primitive.ByteArray.LittleEndian
  ( -- * Aligned
    writeByteArray
  , readByteArray
  , indexByteArray
    -- * Unaligned
  , writeUnalignedByteArray
  , readUnalignedByteArray
  , indexUnalignedByteArray
  ) where

import Data.Primitive (Prim,MutableByteArray,ByteArray)
import Control.Monad.Primitive (PrimState,PrimMonad)
import System.ByteOrder (Bytes,toLittleEndian,fromLittleEndian)
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned)
import qualified Data.Primitive as PM
import qualified Data.Primitive.ByteArray.Unaligned as PMU

writeByteArray :: (PrimMonad m, Prim a, Bytes a) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeByteArray arr ix v = PM.writeByteArray arr ix (toLittleEndian v)

readByteArray :: (PrimMonad m, Prim a, Bytes a) => MutableByteArray (PrimState m) -> Int -> m a
readByteArray arr ix = fromLittleEndian <$> PM.readByteArray arr ix

indexByteArray :: (Prim a, Bytes a) => ByteArray -> Int -> a
indexByteArray arr ix = fromLittleEndian (PM.indexByteArray arr ix)

writeUnalignedByteArray :: (PrimMonad m, PrimUnaligned a, Bytes a)
  => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeUnalignedByteArray arr ix v = PMU.writeUnalignedByteArray arr ix (toLittleEndian v)

readUnalignedByteArray :: (PrimMonad m, PrimUnaligned a, Bytes a)
  => MutableByteArray (PrimState m) -> Int -> m a
readUnalignedByteArray arr ix = fromLittleEndian <$> PMU.readUnalignedByteArray arr ix

indexUnalignedByteArray :: (PrimUnaligned a, Bytes a)
  => ByteArray -> Int -> a
indexUnalignedByteArray arr ix = fromLittleEndian (PMU.indexUnalignedByteArray arr ix)

