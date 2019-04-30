{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit.Eval where

import           Circuit.Class
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import           Data.IORef
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import qualified Data.Sequence        as Seq
import           System.ZMQ4.Monadic
import           Utils


initCircuit :: Key -> (forall z . ZMQ z (Context z))
initCircuit k =
  liftIO
    $   Context
    <$> pure False
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef M.empty
    <*> newIORef Seq.empty
    <*> pure Nothing
    <*> pure k
    <*> genOffset 16

initCircuitRemote :: Key -> (forall z . Socket z Rep -> ZMQ z (Context z))
initCircuitRemote k z = do
  ctx <- initCircuit k
  return $ ctx { isRemote = True, zmqSocket = Just z }

getCircuit :: forall a . (forall z . Builder z a) -> IO Int
getCircuit c = runZMQ $ do
  context <- initCircuit ""
  runReaderT (runBuilder c >> go) context
 where
  go :: ReaderT (Context z) (ZMQ z) Int
  go = do
    circ <- asks circuit
    cir  <- liftIO (readIORef circ)
    return $ Seq.length cir


evalGate :: BasicHashMap Int Key -> Gate -> IO ()
evalGate hash (Const wire k) = do
  query <- lookupHash hash wire
  case query of
    Nothing  -> insertHash hash wire k
    (Just _) -> return ()
evalGate hash (Free (i0, i1) o) = do
  ki0 <- fromJust <$> lookupHash hash i0
  ki1 <- fromJust <$> lookupHash hash i1
  liftIO $ insertHash hash o (ki0 `xorKey` ki1)
evalGate hash (Half _ (i0, i1) o (k0, k1)) = do
  ki0 <- fromJust <$> lookupHash hash i0
  ki1 <- fromJust <$> lookupHash hash i1
  -- get r⊕b/color bit from wire b
  let colorA       = getColor ki0
      rb           = getColor ki1
      (aesa, aesb) = (initAES ki0, initAES ki1)
      g            = case colorA of
        0 -> ctrCombine aesa nullIV (BS.replicate 16 0)
        1 -> ctrCombine aesa nullIV k0
      e = case rb of
        0 -> ctrCombine aesb nullIV (BS.replicate 16 0)
        1 -> ctrCombine aesb nullIV k1 `xorKey` ki0
      out = g `xorKey` e
  insertHash hash o out
evalGate hash g = do
  let (in0, in1)               = inputs g
      out                      = outs g
      (row0, row1, row2, row3) = table g
  ki0 <- fromJust <$> lookupHash hash in0
  ki1 <- fromJust <$> lookupHash hash in1
  let row          = getRowFromKeys ki0 ki1
      (aes0, aes1) = (initAES ki0, initAES ki1)
      ko           = doubleDecrypt aes0 aes1 $ case row of
        0 -> row0
        1 -> row1
        2 -> row2
        3 -> row3
        _ -> error "wrong color bits!"
  insertHash hash out ko
  where doubleDecrypt a b = ctrCombine a nullIV . ctrCombine b nullIV

eval :: Key -> Key -> (forall z . Builder z [Int]) -> IO Key
eval a b c = runZMQ $ do
  context    <- initCircuit b -- assume Bob is client
  (out, ctx) <- flip runReaderT context $ runBuilder c >>= \os ->
    (,) <$> pure os <*> ask
  circ  <- liftIO $ readIORef (circuit ctx)
  wires <- liftIO $ readIORef (wireMap ctx)
  let in0 = bytesToBits . BS.unpack $ a
      in1 = bytesToBits . BS.unpack $ b
  wireNum <- liftIO $ readIORef (wireIdx ctx)
  hash    <- liftIO newBasicHash
  -- setup initial state of input wires
  forM_ [0 .. length in0 - 1] $ \idx -> case M.lookup idx wires of
    Nothing -> error "input length exceed circuit's input"
    (Just (k0, k1)) ->
      liftIO $ insertHash hash idx (if in0 !! idx then k1 else k0)
  forM_ [0 .. length in1 - 1] $ \idx -> do
    let wireIndex = idx + length in0
    case M.lookup wireIndex wires of
      Nothing -> error "input length exceed circuit's input"
      (Just (k0, k1)) ->
        liftIO $ insertHash hash wireIndex (if in1 !! idx then k1 else k0)
  -- evaluate all gates
  -- mapM_ (evalGate state) circ
  void . liftIO $ traverse (evalGate hash) circ
  outBits <- forM out $ \i -> do
    k <- liftIO $ fromJust <$> lookupHash hash i
    let (k0, _) = wires M.! i
    return $ k /= k0
  return (BS.pack . bitsToBytes $ outBits)
