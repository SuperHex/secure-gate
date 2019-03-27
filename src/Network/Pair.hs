{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Network.Pair where

import           Circuit
import           Control.Monad
import           Control.Monad.Reader
import           Crypto.Cipher.Types     (ctrCombine, nullIV)
import           Data.Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS (fromStrict, toStrict)
import qualified Data.HashTable.IO       as H
import qualified Data.HashTable.ST.Basic as Basic (HashTable)
import           Data.IORef
import           Data.List.NonEmpty      (fromList, toList, NonEmpty)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           Data.String             (fromString)
import           System.ZMQ4.Monadic

type Address = String
type BasicHashMap k v = H.IOHashTable (Basic.HashTable) k v

initServer :: Address -> forall z . ZMQ z (Socket z Rep)
initServer addr = do
  sock <- socket Rep
  liftIO . putStrLn $ "starting server at: " ++ addr
  liftIO . putStrLn $ "waiting client ..."
  bind sock addr
  return sock

initClient :: Address -> forall z . ZMQ z (Socket z Req)
initClient addr = do
  sock <- socket Req
  liftIO . putStrLn $ "connecting to: " ++ addr
  liftIO . putStrLn $ "waiting server ..."
  connect sock addr
  return sock

runServer :: Builder [Int] -> Key -> (forall z . Socket z Rep -> ZMQ z ())
runServer prog msg sock = do
  -- 1. initialize context
  ctx        <- liftIO $ initCircuitRemote sock msg
  -- 2. build and send circuit lazily
  (out, ref) <- flip runReaderT
                     ctx
                     (prog >>= \o -> liftM2 (,) (pure o) (asks wireMap))
  -- 3. tell the client that all gates are sent
  void $ receive sock
  send sock [] "SIGTERM"
  -- 4. tell the client which wires are outputs
  void $ receive sock
  sendMulti sock (fromList $ fmap (LBS.toStrict . encode) out)
  -- 5. give the keys
  wires <- liftIO $ readIORef ref
  let out' = fmap (wires M.!) out
  void $ receive sock
  sendMulti sock (fromList $ fmap (LBS.toStrict . encode) out')
  -- 6. receive result from client
  result <- receive sock
  send sock [] ""
  liftIO . putStrLn $ "Receved result: " ++ show
    (decode @Int (LBS.fromStrict result))


runClient :: Key -> (forall z . Socket z Req -> ZMQ z ())
runClient msg sock = do
  -- 1. request Alice's input
  send sock [] "give alice's input"
  alice <- receiveMulti sock

  -- 2. request Bob's input through oblivious transfer
  sendMulti
    sock
    (fromList . fmap (LBS.toStrict . encode) . toBits . BS.unpack $ msg)
  bob <- receiveMulti sock

  -- 3. initialize the environment
  hm  <- liftIO $ H.new @Basic.HashTable
  liftIO $ forM_ (zip [0 :: Int ..] alice) $ \(num, bit) -> do
    -- assume received keys are ordered
    H.insert hm num bit
  liftIO $ forM_ (zip [length alice ..] bob) $ \(num, bit) -> do
    H.insert hm num bit

  -- 4. evaluate received gates until receive SIGTERM
  whileM_ (send sock [] "gates" >> receive sock) (/= "SIGTERM") $ \gate -> do
    let gate' = decodeOrFail @Gate (LBS.fromStrict gate)
    case gate' of
      (Left  (_, _, e)) -> fail e
      (Right (_, _, g)) -> do
        liftIO . putStrLn $ "Received " ++ show g
        evalGateR hm g

  -- 5. retrive output wires
  send sock [] "out"
  out <- receiveMulti sock
  let outWires = fmap (decode @Int . LBS.fromStrict) out
  outVal <- liftIO $ mapM (fmap fromJust . H.lookup hm) outWires

  -- 6. retrive real value
  send sock [] "dict"
  keys <- receiveMulti sock
  let ks = fmap (decode @(Key, Key) . LBS.fromStrict) keys
      result' =
        fromFiniteBits @Int $ fmap (\(b, (lo, _)) -> b /= lo) (zip outVal ks)
  -- 7. send back results
  send sock [] (LBS.toStrict $ encode (result' :: Int))
  void $ receive sock
  -- print result
  liftIO $ print result'
 where
  whileM_ :: Monad m => m a -> (a -> Bool) -> (a -> m b) -> m ()
  whileM_ pred test act = do
    a <- pred
    when (test a) (act a >> whileM_ pred test act)

evalGateR :: BasicHashMap Int Key -> Gate -> ZMQ z ()
evalGateR hash (Const _ wire k) = liftIO $ H.insert hash wire k
evalGateR hash g                = do
  let (in0, in1)               = inputs g
      out                      = outs g
      (row0, row1, row2, row3) = table g
  ki0 <- liftIO $ fromJust <$> H.lookup hash in0
  ki1 <- liftIO $ fromJust <$> H.lookup hash in1
  let row          = getRowFromKeys ki0 ki1
      (aes0, aes1) = (initAES ki0, initAES ki1)
      ko           = doubleDecrypt aes0 aes1 $ case row of
        0 -> row0
        1 -> row1
        2 -> row2
        3 -> row3
        _ -> error "wrong color bits!"
  liftIO $ H.insert hash out ko
  where doubleDecrypt a b = ctrCombine a nullIV . ctrCombine b nullIV
