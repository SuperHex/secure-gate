{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Circuit.Wire where

import           Circuit.Class
import           Control.Monad        (replicateM)
import           Control.Monad.Reader
import           Data.Binary          (decode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import           Data.IORef
import           Data.List.NonEmpty   (fromList)
import qualified Data.Map             as M
import           System.ZMQ4.Monadic
import           Utils                (bytesToBits)

data Party = Alice | Bob

{-# INLINE in8 #-}
in8 :: Party -> Builder z [Int]
in8 = inN 8

{-# INLINE in16 #-}
in16 :: Party -> Builder z [Int]
in16 = inN 16

{-# INLINE in32 #-}
in32 :: Party -> Builder z [Int]
in32 = inN 32

{-# INLINE in64 #-}
in64 :: Party -> Builder z [Int]
in64 = inN 64

{-# INLINE inN #-}
inN :: Int -> Party -> Builder z [Int]
inN n p = do
  w <- replicateM n mkWire
  case p of
    Alice -> sendLocalInput w
    Bob   -> replyOT w
  return w

sendLocalInput :: [Int] -> Builder z ()
sendLocalInput ws = Builder $ do
  zmq <- asks zmqSocket
  case zmq of
    Nothing     -> return ()
    (Just sock) -> do
      inpt    <- asks localInput
      wireRef <- asks wireMap
      wires   <- liftIO $ readIORef wireRef
      let bin  = bytesToBits . BS.unpack $ inpt
          keys = flip fmap (zip ws bin) $ \(idx, bit) ->
            let (lo, hi) = wires M.! idx in if bit then hi else lo
      void . lift . receive $ sock
      lift $ sendMulti sock (fromList keys)

replyOT :: [Int] -> Builder z ()
replyOT ws = Builder $ do
  zmq <- asks zmqSocket
  case zmq of
    Nothing     -> return ()
    (Just sock) -> do
      query <- lift $ receiveMulti sock
      ref   <- asks wireMap
      wires <- liftIO $ readIORef ref
      let bin  = fmap (decode @Bool . LBS.fromStrict) query
          keys = flip fmap (zip ws bin) $ \(idx, bit) ->
            let (lo, hi) = wires M.! idx in if bit then hi else lo
      lift $ sendMulti sock (fromList keys)
