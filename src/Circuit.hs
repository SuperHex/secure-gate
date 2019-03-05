{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Circuit where

import           Control.Monad
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random        (getRandomBytes)
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString      as BS
import           Data.IORef
import qualified Data.Map             as M
import qualified Data.Sequence        as Seq
import           Data.Word

type Key = BS.ByteString
type Message = BS.ByteString
type CipherText = BS.ByteString

encrypt :: Key -> Message -> CipherText
encrypt k m = undefined

genAESKey
  :: Int     -- | key length (bytes)
  -> IO Key
genAESKey = getRandomBytes

genAESKeyPair
  :: Int           -- | key length (bytes)
  -> IO (Key, Key)
genAESKeyPair size = do
  k0 <- genAESKey size
  k1 <- genAESKey size
  c  <- genAESKey 16
  -- sample only the last bit
  let color = (.&. 0x01) . BS.last $ c
      setColor pos =
        BS.pack
          . (\x -> init x ++ [clearBit (last x) 0 `xor` color `xor` pos])
          . BS.unpack
  return (setColor 0 k0, setColor 1 k1)


genAESKeyPair128 = genAESKeyPair 16 -- 128 `div` 8
genAESKeyPair256 = genAESKeyPair 32 -- 256 `div` 8

getColorBit :: Wire -> Word8
getColorBit (k0, _) = (.&. 0x01) . BS.last $ k0

getRowFromKeys :: Key -> Key -> Word8
getRowFromKeys k0 k1 =
  (getColorBit (k0, undefined) `shiftL` 1) + getColorBit (k1, undefined)

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


-- newtype TruthTable = TT { evalTable :: (Key, Key, Key, Key) }
type Wire = (Key, Key)
type Table = (Key, Key, Key, Key)

initCipher :: forall c . (BlockCipher c) => Key -> Either CryptoError c
initCipher k = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed c -> Right c

initAES :: Key -> AES128
initAES k = case cipherInit k of
  CryptoFailed e -> error (show e)
  CryptoPassed c -> c

------------------------------------

data Gate = Gate
  { index :: Int
  , inputs :: (Int, Int)
  , outs  :: Int
  , table :: (Key, Key, Key, Key)
  }

instance Show Gate where
  show (Gate idx ipt out _) =
    "Gate " ++ show idx ++ ": " ++ show ipt ++ " -> " ++ show out

data GateType = AND | XOR | OR | NAND
  deriving Show

data Context = Context
  { gateIdx :: IORef Int
  , wireIdx :: IORef Int
  , wireMap  :: IORef (M.Map Int (Key, Key))
  , circuit :: IORef Circuit
  }

type Circuit = Seq.Seq Gate
type Builder a = ReaderT Context IO a

initCircuit :: IO Context
initCircuit =
  Context
    <$> newIORef 1
    <*> newIORef 1
    <*> newIORef M.empty
    <*> newIORef Seq.empty

getCircuit :: Builder a -> IO Circuit
getCircuit c = do
  context <- initCircuit
  flip runReaderT context $ c >> do
    circ <- asks circuit
    lift (readIORef circ)

mkWire :: Builder Int
mkWire = do
  ref <- asks wireIdx
  idx <- lift (readIORef ref)
  lift (modifyIORef' ref (+ 1))
  keys   <- lift genAESKeyPair128
  mapRef <- asks wireMap
  lift $ modifyIORef mapRef (M.insert idx keys)
  return idx

mkGate :: GateType -> Int -> Int -> Builder Int
mkGate t in0 in1 = do
  context <- ask
  -- add gate index by one
  idx     <- lift (readIORef (gateIdx context))
  lift (modifyIORef' (gateIdx context) (+ 1))
  out <- mkWire
  map <- lift (readIORef (wireMap context))
  let w0@(ki00, ki01)              = map M.! in0
      w1@(ki10, ki11)              = map M.! in1
      (   ko0 , ko1 )              = map M.! out
      [aes00, aes01, aes10, aes11] = fmap initAES [ki00, ki01, ki10, ki11]
      (row0, row1, row2, row3) =
        ((aes00, aes10), (aes00, aes11), (aes01, aes10), (aes01, aes11))
      c0     = getColorBit w0
      c1     = getColorBit w1
      cipher = case t of
        AND ->
          ( doubleEnc row0 ko0
          , doubleEnc row1 ko0
          , doubleEnc row2 ko0
          , doubleEnc row3 ko1
          )
        OR ->
          ( doubleEnc row0 ko0
          , doubleEnc row1 ko1
          , doubleEnc row2 ko1
          , doubleEnc row3 ko1
          )
        XOR ->
          ( doubleEnc row0 ko0
          , doubleEnc row1 ko1
          , doubleEnc row2 ko1
          , doubleEnc row3 ko0
          )
        NAND ->
          ( doubleEnc row0 ko1
          , doubleEnc row1 ko1
          , doubleEnc row2 ko1
          , doubleEnc row3 ko0
          )
      gate = Gate idx (in0, in1) out (shuffle c0 c1 cipher)
  lift (modifyIORef' (circuit context) (Seq.|> gate))
  return out
 where
  doubleEnc :: (AES128, AES128) -> Key -> Key
  doubleEnc (a, b) = ecbEncrypt a . ecbEncrypt b

notGate :: Int -> Builder Int
notGate x = mkGate NAND x x
