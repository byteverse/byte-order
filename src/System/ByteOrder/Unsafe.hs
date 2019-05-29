{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}
{-# language TypeApplications #-}

module System.ByteOrder.Unsafe
  ( Fixed(..)
  , FixedOrdering(..)
  ) where

-- TODO: There is no reason to label this module as unsafe anymore.

import Data.Kind (Type)
import Data.Primitive.Types (Prim)
import GHC.ByteOrder (ByteOrder(BigEndian,LittleEndian))
import System.ByteOrder.Class (Bytes(toLittleEndian,toBigEndian))

import qualified Data.Primitive.Types as PM

-- | A word whose byte order is specified (not platform dependent)
-- when working with 'Prim' and 'Storable'.
newtype Fixed :: ByteOrder -> Type -> Type where
  Fixed :: { getFixed :: a } -> Fixed b a

type role Fixed phantom representational

-- | A byte order that can be interpreted as a conversion function.
-- This class is effectively closed. The only instances are for
-- 'BigEndian' and 'LittleEndian'. It is not possible to write more
-- instances since there are no other inhabitants of 'ByteOrder'.
class FixedOrdering (b :: ByteOrder) where
  toFixedEndian :: Bytes a => a -> a

instance FixedOrdering 'LittleEndian where
  toFixedEndian = toLittleEndian

instance FixedOrdering 'BigEndian where
  toFixedEndian = toBigEndian

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
