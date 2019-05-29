{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Data.Primitive.ByteArray
import Data.Word
import GHC.Exts (RealWorld)
import System.ByteOrder

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "B"
  testB
  putStrLn "C"
  testC
  putStrLn "Finished"

testA :: IO ()
testA = do
  let payload = 0x01234567 :: Word32
  marr <- newByteArray 4
  setByteArray marr 0 4 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'LittleEndian payload)
  let name = "testA"
  expectByte name marr 0 0x67
  expectByte name marr 1 0x45
  expectByte name marr 2 0x23
  expectByte name marr 3 0x01

testB :: IO ()
testB = do
  let payload = 0x01234567 :: Word32
  marr <- newByteArray 4
  setByteArray marr 0 4 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'BigEndian payload)
  let name = "testB"
  expectByte name marr 0 0x01
  expectByte name marr 1 0x23
  expectByte name marr 2 0x45
  expectByte name marr 3 0x67

testC :: IO ()
testC = do
  let payload = 0x0123456789ABCDEF :: Word64
  marr <- newByteArray 8
  setByteArray marr 0 8 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'BigEndian payload)
  let name = "testC"
  expectByte name marr 0 0x01
  expectByte name marr 1 0x23
  expectByte name marr 2 0x45
  expectByte name marr 3 0x67
  expectByte name marr 4 0x89
  expectByte name marr 5 0xAB
  expectByte name marr 6 0xCD
  expectByte name marr 7 0xEF

expectByte :: String -> MutableByteArray RealWorld -> Int -> Word8 -> IO ()
expectByte name marr ix w = do
  v <- readByteArray marr ix
  if v == w
    then pure ()
    else fail (name ++ ": byte " ++ show ix ++ " was wrong")

