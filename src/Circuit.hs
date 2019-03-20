{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}

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
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.List.Split      (chunksOf)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Mutable  as MV
import           Data.Word
import           GHC.Generics         (Generic)
import           System.ZMQ4.Monadic

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
  } deriving (Generic)

instance Binary Gate

instance Show Gate where
  show (Gate idx ipt out _) =
    "Gate " ++ show idx ++ ": " ++ show ipt ++ " -> " ++ show out

data GateType = AND | XOR | OR | NAND
  deriving Show

data Context z =  Context
  { isRemote :: Bool
  , gateIdx :: IORef Int
  , wireIdx :: IORef Int
  , wireMap :: IORef (M.Map Int Wire)
  , circuit :: IORef Circuit
  , zmqSocket :: Maybe ((Socket z Rep))
  , localInput :: Key
  }

type Circuit = Seq.Seq Gate
type Builder a = forall z . ReaderT (Context z) (ZMQ z) a

initCircuit :: Key -> IO (Context z)
initCircuit k =
  Context
    <$> pure False
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef M.empty
    <*> newIORef Seq.empty
    <*> pure Nothing
    <*> pure k

initCircuitRemote z k = do
  ctx <- initCircuit k
  return $ ctx { isRemote = True, zmqSocket = Just z }

getCircuit :: Builder a -> Key -> IO Circuit
getCircuit c k = runZMQ $ do
  context <- liftIO $ initCircuit k
  flip runReaderT context $ c >> do
    circ <- asks circuit
    liftIO (readIORef circ)

evalCircuit :: (FiniteBits b) => Circuit -> M.Map Int Wire -> b -> b -> b
evalCircuit = undefined

evalGate :: MV.IOVector Key -> Gate -> IO (MV.IOVector Key)
evalGate state g = do
  let (in0, in1)               = inputs g
      out                      = outs g
      (row0, row1, row2, row3) = table g
  ki0 <- MV.unsafeRead state in0
  ki1 <- MV.unsafeRead state in1
  let row          = getRowFromKeys ki0 ki1
      (aes0, aes1) = (initAES ki0, initAES ki1)
      ko           = doubleDecrypt aes0 aes1 $ case row of
        0 -> row0
        1 -> row1
        2 -> row2
        3 -> row3
        _ -> error "wrong color bits!"
  MV.unsafeWrite state out ko
  return state
  where doubleDecrypt a b = ctrCombine a nullIV . ctrCombine b nullIV

eval :: Key -> Key -> Builder [Int] -> IO Key
eval a b c = runZMQ $ do
  context    <- liftIO $ initCircuit b -- assume Bob is client
  (out, ctx) <- flip runReaderT context $ c >>= \outs ->
    (,) <$> pure outs <*> ask
  circ  <- liftIO $ readIORef (circuit ctx)
  wires <- liftIO $ readIORef (wireMap ctx)
  let in0 = toBits . BS.unpack $ a
      in1 = toBits . BS.unpack $ b
  wireNum <- liftIO $ readIORef (wireIdx ctx)
  state   <- liftIO $ MV.new wireNum
  -- setup initial state of input wires
  forM_ [0 .. length in0 - 1] $ \idx -> do
    case M.lookup idx wires of
      Nothing -> error "input length exceed circuit's input"
      (Just (k0, k1)) ->
        liftIO $ MV.unsafeWrite state idx (if in0 !! idx then k1 else k0)
  forM_ [0 .. length in1 - 1] $ \idx -> do
    let wireIndex = idx + length in0
    case M.lookup wireIndex wires of
      Nothing -> error "input length exceed circuit's input"
      (Just (k0, k1)) ->
        liftIO $ MV.unsafeWrite state wireIndex (if in1 !! idx then k1 else k0)
  -- evaluate all gates
  -- mapM_ (evalGate state) circ
  liftIO $ foldM_ evalGate state circ
  outBits <- forM out $ \i -> do
    k <- liftIO $ MV.read state i
    let (k0, _) = wires M.! i
    return $ k /= k0
  return (BS.pack . fromBits $ outBits)

fromBits :: [Bool] -> [Word8]
fromBits = fmap (foldr f zeroBits) . chunksOf 8
  where f b x = if b then setBit (x `shiftL` 1) 0 else x `shiftL` 1

toBits :: [Word8] -> [Bool]
toBits = concatMap (\word -> fmap (testBit word) [0 .. finiteBitSize word - 1])

finiteToBits :: (FiniteBits b) => b -> [Bool]
finiteToBits b = fmap (testBit b) [0 .. finiteBitSize b - 1]

fromFiniteBits :: (FiniteBits b) => [Bool] -> b
fromFiniteBits = foldr f zeroBits
  where f b x = if b then setBit (x `shiftL` 1) 0 else x `shiftL` 1

mkWire :: Builder Int
mkWire = do
  ref <- asks wireIdx
  idx <- liftIO (readIORef ref)
  liftIO (modifyIORef' ref (+ 1))
  keys   <- liftIO genAESKeyPair128
  mapRef <- asks wireMap
  liftIO $ modifyIORef mapRef (M.insert idx keys)
  return idx

mkGate :: GateType -> Int -> Int -> Builder Int
mkGate t in0 in1 = do
  context <- ask
  -- add gate index by one
  idx     <- liftIO (readIORef (gateIdx context))
  liftIO (modifyIORef' (gateIdx context) (+ 1))
  out <- mkWire
  map <- liftIO (readIORef (wireMap context))
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
  liftIO (modifyIORef' (circuit context) (Seq.|> gate))

  remote <- asks isRemote
  when remote $ do
    sock <- fromJust <$> asks zmqSocket
    void . lift $ receive sock  -- just ignore whatever client says
    lift $ send sock [] (LBS.toStrict . encode $ gate) -- and send more gates!
  return out
 where
  doubleEnc :: (AES128, AES128) -> Key -> Key
  doubleEnc (a, b) = ctrCombine a nullIV . ctrCombine b nullIV

notGate :: Int -> Builder Int
notGate x = mkGate NAND x x
