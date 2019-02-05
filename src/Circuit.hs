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
genAESKeyPair size = liftM2 (,) (genAESKey size) (genAESKey size)

genAESKeyPair256 = genAESKeyPair (256 `div` 8)

newtype TruthTable = TT { evalTable :: (Key, Key, Key, Key) }
type Wire = (Key, Key)

initCipher :: forall c . (BlockCipher c) => Key -> Either CryptoError c
initCipher k = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed c -> Right c

initAES :: Key -> AES256
initAES k = case cipherInit k of
  CryptoFailed e -> error (show e)
  CryptoPassed c -> c

------------------------------------

data Gate = Gate
  { index :: Int
  , inputs :: (Int, Int)
  , outs  :: Int
  , table :: (Key, Key, Key, Key)
  } deriving (Show)

data GateType = AND | XOR
  deriving Show

data Context = Context
  { gateIdx :: IORef Int
  , wireIdx :: IORef Int
  , wireMap  :: IORef (M.Map Int (Key, Key))
  , circuit :: IORef Circuit
  }

type Circuit = Seq.Seq Gate
type Builder a = ReaderT Context IO a

getCircuit :: Builder a -> IO Circuit
getCircuit c = do
  gate <- newIORef 1
  wire <- newIORef 1
  map  <- newIORef M.empty
  cir  <- newIORef Seq.empty
  let context = Context gate wire map cir
  flip runReaderT context $ c >> do
    circ <- asks circuit
    lift (readIORef circ)

mkWire :: Builder Int
mkWire = do
  ref <- asks wireIdx
  idx <- lift (readIORef ref)
  lift (modifyIORef' ref (+ 1))
  keys   <- lift genAESKeyPair256
  mapRef <- asks wireMap
  lift $ modifyIORef mapRef (M.insert idx keys)
  return idx

mkGate :: GateType -> (Int, Int) -> Int -> Builder Gate
mkGate t input@(in0, in1) out = do
  context <- ask
  -- add gate index by one
  idx     <- lift (readIORef (gateIdx context))
  lift (modifyIORef' (gateIdx context) (+ 1))
  map <- lift (readIORef (wireMap context))
  let (ki00, ki01)                 = map M.! in0
      (ki10, ki11)                 = map M.! in1
      (ko0 , ko1 )                 = map M.! out
      [aes00, aes01, aes10, aes11] = fmap initAES [ki00, ki01, ki10, ki11]
      (row0, row1, row2, row3) =
        ((aes00, aes10), (aes00, aes11), (aes01, aes10), (aes01, aes11))
      cipher = case t of
        AND ->
          ( doubleEnc row0 ko0
          , doubleEnc row1 ko0
          , doubleEnc row2 ko0
          , doubleEnc row3 ko1
          )
        XOR ->
          ( doubleEnc row0 ko0
          , doubleEnc row1 ko1
          , doubleEnc row2 ko1
          , doubleEnc row3 ko0
          )
      gate = Gate idx input out cipher
  lift (modifyIORef' (circuit context) (Seq.|> gate))
  return gate
  where doubleEnc (a, b) = ecbEncrypt a . ecbEncrypt b

