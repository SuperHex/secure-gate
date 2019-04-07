{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit where

import           Control.Monad
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random                (getRandomBytes)
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.IORef
import           Data.List.Split              (chunksOf)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import qualified Data.Sequence                as Seq
import qualified Data.Vector.Mutable          as MV
import           GHC.Generics                 (Generic)
import           System.ZMQ4.Monadic

type Key = BS.ByteString
type Message = BS.ByteString
type CipherText = BS.ByteString

genAESKey :: Int -> IO Key
genAESKey = getRandomBytes

setColor :: Word8 -> Word8 -> Key -> Key
setColor color pos =
  BS.pack
    . (\x -> init x ++ [clearBit (last x) 0 `xor` color `xor` pos])
    . BS.unpack

genColor :: IO Word8
genColor = (.&. 0x01) . BS.last <$> genAESKey 16

genAESKeyPair :: Int -> IO (Key, Key)
genAESKeyPair size = do
  k0    <- genAESKey size
  k1    <- genAESKey size
  color <- genColor
  return (setColor color 0 k0, setColor color 1 k1)

genOffset :: Int -> IO Key
genOffset size = do
  r <- genAESKey size
  return $ setColor 1 0 r

xorKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorKey a = BS.pack . BS.zipWith xor a

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

genAESKeyPair128With = genAESKeyPairWith 16
genAESKeyPair256With = genAESKeyPairWith 32

getColorBit :: Key -> Word8
getColorBit = (.&. 0x01) . BS.last

getRowFromKeys :: Key -> Key -> Word8
getRowFromKeys k0 k1 = (getColorBit k0 `shiftL` 1) + getColorBit k1

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

data Gate
  = Const Int Int Key -- Const (gate index) (output wire index) (value)
  | Free (Int, Int) Int
  | Half Int (Int, Int) Int (Key, Key)
  | Logic { index :: Int
          , inputs :: (Int, Int)
          , outs  :: Int
          , table :: (Key, Key, Key, Key) }
  deriving (Generic)

instance Binary Gate

instance Show Gate where
  show (Logic idx ipt out _) =
    "Gate " ++ show idx ++ ": " ++ show ipt ++ " -> " ++ show out
  show (Const idx wire _) = "ConstBit " ++ show idx ++ ": wire[" ++ show wire ++ "]"
  show (Free i o) = "Free " ++ show i ++ " -> " ++ show o

data Context z =  Context
  { isRemote :: Bool
  , gateIdx :: IORef Int
  , wireIdx :: IORef Int
  , wireMap :: IORef (M.Map Int Wire)
  , circuit :: IORef Circuit
  , zmqSocket :: Maybe (Socket z Rep)
  , localInput :: Key
  , freeOffset :: Key
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
    <*> genOffset 16

initCircuitRemote :: Socket z Rep -> Key -> IO (Context z)
initCircuitRemote z k = do
  ctx <- initCircuit k
  return $ ctx { isRemote = True, zmqSocket = Just z }

getCircuit :: Builder a -> IO Int
getCircuit c = runZMQ $ do
  context <- liftIO $ initCircuit ""
  flip runReaderT context $ c >> do
    circ <- asks circuit
    cir  <- liftIO (readIORef circ)
    return $ Seq.length cir

evalCircuit :: (FiniteBits b) => Circuit -> M.Map Int Wire -> b -> b -> b
evalCircuit = undefined

evalGate :: MV.IOVector Key -> Gate -> IO (MV.IOVector Key)
evalGate state (Const _ wire key) = do
  MV.unsafeWrite state wire key
  return state
evalGate state (Free (i0, i1) o) = do
  ki0 <- MV.unsafeRead state i0
  ki1 <- MV.unsafeRead state i1
  let out = ki0 `xorKey` ki1
  MV.unsafeWrite state o out
  return state
evalGate state (Half _ (i0, i1) o (k0, k1)) = do
  ki0 <- MV.unsafeRead state i0
  ki1 <- MV.unsafeRead state i1
  -- get r⊕b/color bit from wire b
  let colorA       = getColorBit ki0
      rb           = getColorBit ki1
      (aesa, aesb) = (initAES ki0, initAES ki1)
      g            = case colorA of
        0 -> ctrCombine aesa nullIV (BS.replicate 16 0)
        1 -> ctrCombine aesa nullIV k0
      e = case rb of
        0 -> ctrCombine aesb nullIV (BS.replicate 16 0)
        1 -> ctrCombine aesb nullIV k1 `xorKey` ki0
      out = g `xorKey` e
  MV.unsafeWrite state o out
  return state
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
  (out, ctx) <- flip runReaderT context $ c >>= \os -> (,) <$> pure os <*> ask
  circ       <- liftIO $ readIORef (circuit ctx)
  wires      <- liftIO $ readIORef (wireMap ctx)
  let in0 = toBits . BS.unpack $ a
      in1 = toBits . BS.unpack $ b
  wireNum <- liftIO $ readIORef (wireIdx ctx)
  state   <- liftIO $ MV.new wireNum
  -- setup initial state of input wires
  forM_ [0 .. length in0 - 1] $ \idx -> case M.lookup idx wires of
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
fromBits = fmap fromFiniteBits . chunksOf 8

toBits :: [Word8] -> [Bool]
toBits = concatMap finiteToBits

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
  offset <- asks freeOffset
  keys   <- liftIO $ genAESKeyPair128With offset
  -- keys   <- liftIO $ genAESKeyPair128
  mapRef <- asks wireMap
  liftIO $ modifyIORef mapRef (M.insert idx keys)
  return idx

halfAND :: Int -> Int -> Builder Int
halfAND a b = do
  context <- ask
  idx     <- liftIO $ readIORef (gateIdx context)
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  maskMap <- liftIO $ readIORef (wireMap context)
  wIdx    <- liftIO $ readIORef (wireIdx context)
  liftIO $ modifyIORef (wireIdx context) (+ 1)
  -- select `r` be the color bit of wire b
  let
    (a0, a1)      = maskMap M.! a
    (b0, b1)      = maskMap M.! b
    colorA        = getColorBit a0
    r             = getColorBit b0
    (aes0, aes1)  = (initAES a0, initAES a1)
    (aes2, aes3)  = (initAES b0, initAES b1)
    offset        = freeOffset context
    (cg, gCipher) = case colorA of
      0 ->
        -- garbler half gate
        -- order: Enc_{A}(C)   <-- becomes {0}^n when color(a) == 0
        --        Enc_{A⊕R}(C⊕(r * R))
        --        C := Dec_{B}({0}^n)
        let c0  = ctrCombine aes0 nullIV (BS.replicate 16 0)
            out = ctrCombine aes1 nullIV $ case r of
              0 -> c0
              1 -> c0 `xorKey` offset
        in  (c0, out)
      1 -> case r of
        0 ->
          let c0  = ctrCombine aes1 nullIV (BS.replicate 16 0)
              out = ctrCombine aes0 nullIV c0
          in  (c0, out)
        1 ->
          let c1  = ctrCombine aes1 nullIV (BS.replicate 16 0)
              out = ctrCombine aes0 nullIV (c1 `xorKey` offset)
          in  (c1 `xorKey` offset, out)
    -- evaluator half gate, no need to permute
    ce = ctrCombine (if r == 0 then aes2 else aes3) nullIV (BS.replicate 16 0)
    eCipher =
      ctrCombine (if r == 0 then aes3 else aes2) nullIV (ce `xorKey` a0)
    outFalse = cg `xorKey` ce
    outTrue  = outFalse `xorKey` offset
    gate     = Half idx (a, b) wIdx (gCipher, eCipher)
  liftIO $ modifyIORef (wireMap context) (M.insert wIdx (outFalse, outTrue))
  unless (isRemote context) $ liftIO $ modifyIORef (circuit context)
                                                   (Seq.|> gate)
  sendGate gate
  return wIdx

freeXOR :: Int -> Int -> Builder Int
freeXOR in0 in1 = do
  context <- ask
  _       <- liftIO $ readIORef (gateIdx context)
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  -- generate output wire C_0 = A⊕B, C_1 = A⊕B⊕R
  --   where A, B := False, R := offset
  wire <- liftIO $ readIORef (wireIdx context)
  liftIO $ modifyIORef (wireIdx context) (+ 1)
  maskMap <- liftIO $ readIORef (wireMap context)
  let (a0, _)   = maskMap M.! in0
      (b0, _)   = maskMap M.! in1
      offset    = freeOffset context
      c@(c0, _) = (a0 `xorKey` b0, c0 `xorKey` offset)
      gate      = Free (in0, in1) wire
  liftIO $ modifyIORef (wireMap context) (M.insert wire c)
  unless (isRemote context) $ liftIO $ modifyIORef (circuit context)
                                                   (Seq.|> gate)
  sendGate gate
  return wire

mkConstBit :: Bool -> Builder Int
mkConstBit b = do
  out     <- mkWire
  context <- ask
  wmap    <- liftIO (readIORef (wireMap context))
  idx     <- liftIO (readIORef (gateIdx context))
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  let (lo, hi) = wmap M.! out
      key      = if b then hi else lo
      gate     = Const idx out key
  -- local evaluation
  unless (isRemote context) $ liftIO $ modifyIORef (circuit context)
                                                   (Seq.|> gate)
  -- remote evaluation
  sendGate gate
  return out

sendGate :: Gate -> Builder ()
sendGate gate = do
  remote <- asks isRemote
  when remote $ do
    sock <- fromJust <$> asks zmqSocket
    void . lift $ receive sock  -- just ignore whatever client says
    lift $ send sock [] (LBS.toStrict . encode $ gate) -- and send more gates!


