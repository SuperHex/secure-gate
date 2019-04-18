{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitForAll #-}

module Utils
  ( module Crypto.Cipher.Types
  , BasicHashMap
  , BasicHashT
  , Key, Wire, Table
  -- hash tables
  , newBasicHash
  , insertHash
  , lookupHash
  -- AES
  , initAES
  , genAESKeyPair128
  , genAESKeyPair128With
  -- color bit
  , genColor
  , getColor
  , setColor
  , getRowFromKeys
  , shuffle
  -- other
  , genOffset
  , xorKey
  ) where

import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random           (getRandomBytes)
import           Data.Bits
import qualified Data.ByteString         as BS
import qualified Data.HashTable.IO       as H
import qualified Data.HashTable.ST.Basic as Basic
import           Data.Word
import           GHC.Prim                (RealWorld)

-- | types for mutable hashtable
type BasicHashMap k v = H.IOHashTable Basic.HashTable k v
type BasicHashT = Basic.HashTable

-- | re-export `H.new`, selecting the basic hashtable
newBasicHash :: IO (Basic.HashTable RealWorld k v)
newBasicHash = H.new @Basic.HashTable

-- | re-export `H.insert`
insertHash = H.insert

-- | re-export `H.lookup`
lookupHash = H.lookup


-- | types for representing key, message, and truth table
type Key = BS.ByteString
type Wire = (Key, Key)
type Table = (Key, Key, Key, Key)

-- | generate AES keys from random bytes. note the input is in bytes
genAESKey :: Int -> IO Key
genAESKey = getRandomBytes

-- | generate AES key pairs, with color bit properly set
genAESKeyPair :: Int -> IO (Key, Key)
genAESKeyPair size = do
  k0    <- genAESKey size
  k1    <- genAESKey size
  color <- genColor
  return (setColor color 0 k0, setColor color 1 k1)

-- | generate AES key pair with length and offset
genAESKeyPairWith :: Int -> Key -> IO (Key, Key)
genAESKeyPairWith size offset = do
  k0    <- genAESKey size
  color <- genColor
  let k0' = setColor color 0 k0
      -- we assume LSB(offset) == 1
      -- in order to generate different label
      -- k1 := k0 ⊕ offset
      k1  = k0' `xorKey` offset
  return (k0', k1)

genAESKeyPair128, genAESKeyPair256 :: IO (Key, Key)
genAESKeyPair128 = genAESKeyPair 16 -- 128 `div` 8
genAESKeyPair256 = genAESKeyPair 32 -- 256 `div` 8

genAESKeyPair128With, genAESKeyPair256With :: Key -> IO (Key, Key)
genAESKeyPair128With = genAESKeyPairWith 16
genAESKeyPair256With = genAESKeyPairWith 32


-- | extract color bit from given key
getColor :: Key -> Word8
getColor = (.&. 0x01) . BS.last

-- | given a random generated color bit and position of the key,
-- set the color bit.
setColor :: Word8 -> Word8 -> Key -> Key
setColor color pos =
  BS.pack
    . (\x -> init x ++ [clearBit (last x) 0 `xor` color `xor` pos])
    . BS.unpack

-- | generate a random color bit (LSB)
genColor :: IO Word8
genColor = (.&. 0x01) . BS.last <$> genAESKey 16

-- | calculate the corresponding row in truth table from two keys
getRowFromKeys :: Key -> Key -> Word8
getRowFromKeys k0 k1 = (getColor k0 `shiftL` 1) + getColor k1

-- | shuffle the truth table based on color bits
-- if c0 == 1, swap line 0 and line 2
-- if c1 == 1, swap line 1 and line 3
shuffle :: Word8 -> Word8 -> Table -> Table
shuffle c0 c1 t@(a, b, c, d) = case c0 of
  0 -> case c1 of
    0 -> t
    1 -> (b, a, d, c)
    _ -> error $ "Error: unknown color bit " ++ show c1
  1 -> case c1 of
    0 -> (c, d, a, b)
    1 -> (d, c, b, a)
    _ -> error $ "Error: unknown color bit " ++ show c1
  _ -> error $ "Error: unknown color bit " ++ show c0

-- | generate a random offset key, which can be used in generating xor gate
genOffset :: Int -> IO Key
genOffset size = do
  r <- genAESKey size
  return $ setColor 1 0 r

-- | bitwise xor two keys
xorKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorKey a = BS.pack . BS.zipWith xor a

initCipher :: forall c . (BlockCipher c) => Key -> Either CryptoError c
initCipher k = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed c -> Right c

-- | initialize an AES key from given bytestring
initAES :: Key -> AES128
initAES k = case cipherInit k of
  CryptoFailed e -> error (show e)
  CryptoPassed c -> c

