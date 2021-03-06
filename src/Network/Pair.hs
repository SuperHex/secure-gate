{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Network.Pair where

import           Circuit.Class
import           Circuit.Eval
import           Control.Monad
import           Control.Monad.Reader
import           Data.Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS (ByteString, fromStrict, toStrict)
import           Data.IORef
import           Data.List.NonEmpty      (fromList)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           System.ZMQ4.Monadic
import           Utils

type Address = String

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
  liftIO . putStrLn $ "connected. now running ..."
  return sock

runServer :: Key -> (forall z . Builder z [Int] -> Socket z Rep -> ZMQ z ())
runServer msg prog sock = do
  -- 1. initialize context
  ctx        <- initCircuitRemote msg sock
  -- 2. build and send circuit lazily
  (out, ref) <- runReaderT
    (runBuilder prog >>= \o -> liftM2 (,) (pure o) (asks wireMap))
    ctx
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


runClient :: Key -> (forall z . Socket z Req -> ZMQ z LBS.ByteString)
runClient msg sock = do
  -- 1. request Alice's input
  send sock [] "give alice's input"
  alice <- receiveMulti sock

  -- 2. request Bob's input through oblivious transfer
  sendMulti
    sock
    (fromList . fmap (LBS.toStrict . encode) . bytesToBits . BS.unpack $ msg)
  bob <- receiveMulti sock

  -- 3. initialize the environment
  hm  <- liftIO newBasicHash
  liftIO $ forM_ (zip [0 :: Int ..] alice) $ uncurry (insertHash hm)
  liftIO $ forM_ (zip [length alice ..] bob) $ uncurry (insertHash hm)

  -- 4. evaluate received gates until receive SIGTERM
  whileM_ (send sock [] "gates" >> receive sock) (/= "SIGTERM") $ \gate -> do
    let gate' = decodeOrFail @Gate (LBS.fromStrict gate)
    case gate' of
      (Left  (_, _, e)) -> fail e
      (Right (_, _, g)) -> evalGateR hm g

  -- 5. retrive output wires
  send sock [] "out"
  out <- receiveMulti sock
  let outWires = fmap (decode @Int . LBS.fromStrict) out
  outVal <- liftIO $ mapM (fmap fromJust . lookupHash hm) outWires

  -- 6. retrive real value
  send sock [] "dict"
  keys <- receiveMulti sock
  let ks      = fmap (decode @(Key, Key) . LBS.fromStrict) keys
      result' = fmap (\(b, (lo, _)) -> b /= lo) (zip outVal ks)
  -- 7. send back results
  send sock [] (LBS.toStrict $ encode (toFinite result' :: Int))
  void $ receive sock
  -- print result
  -- liftIO $ print result'
  pure
    . BB.toLazyByteString
    . BB.byteStringHex
    . BS.pack
    . bitsToBytes
    $ result'
 where
  whileM_ :: Monad m => m a -> (a -> Bool) -> (a -> m b) -> m ()
  whileM_ pred test act = do
    a <- pred
    when (test a) (act a >> whileM_ pred test act)

evalGateR :: BasicHashMap Int Key -> Gate -> ZMQ z ()
evalGateR b g = liftIO $ evalGate b g
