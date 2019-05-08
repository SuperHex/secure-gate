{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Circuit.Class
  (Component (..)
  , View (..)
  , (:~>) (..)
  , Gate (..)
  , Builder (..)
  , Context (..)
  , Circuit
  , freeXORHelper
  , halfANDHelper
  , mkConstBit
  , clearWires
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Generics        as Data
import           Data.IORef
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Proxy
import qualified Data.Sequence        as Seq
import           Data.Word
import qualified GHC.Generics
import           Prelude              hiding (and)
import           System.ZMQ4.Monadic
import           Utils

class (Monad c) => Component (c :: * -> *) a | c -> a where
  high :: c a
  low  :: c a
  mkWire :: c a
  clearWire :: a -> c ()
  xor :: a -> a -> c a
  and :: a -> a -> c a

instance Component (Builder z) Int where
  high = mkConstBit True
  low = mkConstBit False
  mkWire = mkWire'
  clearWire = clearWire'
  xor = freeXOR
  and = halfAND

data (:~>) a b
  = VInt64 [a]
  | VString Int [a]
  deriving (Show, Data.Data)

class (Component c a) => View t c a where
  view :: c [a] -> c (a :~> t)
  unView :: c (a :~> t) -> c [a]

instance View String (Builder z) Int where
  view = fmap (\wires -> VString (length wires `div` 8) wires)
  unView = fmap (\(VString _ ws) -> ws)

instance View Word64 (Builder z) Int where
  view = fmap VInt64
  unView = fmap (\(VInt64 ws) -> ws)

newtype Builder z a = Builder { runBuilder :: ReaderT (Context z) (ZMQ z) a}
  deriving (Functor, Applicative, Monad, MonadIO)

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

data Gate
  = Const Int Key -- Const (output wire index) (value)
  | Free (Int, Int) Int
  | Half Int (Int, Int) Int (Key, Key)
  | Logic { index :: Int
          , inputs :: (Int, Int)
          , outs  :: Int
          , table :: (Key, Key, Key, Key) }
  deriving (GHC.Generics.Generic)

type Circuit = Seq.Seq Gate

instance Binary Gate

instance Show Gate where
  show (Logic idx ipt out _) =
    "Gate " ++ show idx ++ ": " ++ show ipt ++ " -> " ++ show out
  show (Const wire _) = "ConstBit " ++ ": wire[" ++ show wire ++ "]"
  show (Free i o) = "Free " ++ show i ++ " -> " ++ show o

mkWire' :: Builder z Int
mkWire' = Builder $ do
  ref <- asks wireIdx
  idx <- liftIO (readIORef ref)
  liftIO (modifyIORef' ref (+ 1))
  offset <- asks freeOffset
  keys   <- liftIO $ genAESKeyPair128With offset
  -- keys   <- liftIO $ genAESKeyPair128
  mapRef <- asks wireMap
  liftIO $ modifyIORef mapRef (M.insert idx keys)
  return idx

clearWire' :: Int -> Builder z ()
clearWire' wire = Builder $ do
  context <- ask
  wmap    <- liftIO $ readIORef (wireMap context)
  case M.lookup wire wmap of
    Nothing  -> return ()
    (Just _) -> liftIO $ modifyIORef (wireMap context) (M.delete wire)

clearWires :: (Component c a, Functor t, Foldable t) => t a -> c ()
clearWires = mapM_ clearWire

halfANDHelper :: Int -> Int -> Maybe Int -> Builder z Int
halfANDHelper a b o = Builder $ do
  context <- ask
  idx     <- liftIO $ readIORef (gateIdx context)
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  maskMap <- liftIO $ readIORef (wireMap context)
  wIdx    <- case o of
    Nothing -> do
      wIdx <- liftIO $ readIORef (wireIdx context)
      liftIO $ modifyIORef (wireIdx context) (+ 1)
      return wIdx
    (Just widx) -> return widx
  -- select `r` be the color bit of wire b
  let
    (a0, a1) = case M.lookup a maskMap of
      Nothing   -> error $ "halfand: input wire" ++ show a ++ "does not exist"
      (Just a') -> a'
    (b0, b1) = case M.lookup b maskMap of
      Nothing   -> error $ "halfand: input wire" ++ show b ++ "does not exist"
      (Just b') -> b'
    colorA        = getColor a0
    r             = getColor b0
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
  runBuilder $ sendGate gate
  return wIdx

halfAND :: Int -> Int -> Builder z Int
halfAND a b = halfANDHelper a b Nothing

freeXORHelper :: Int -> Int -> Maybe Int -> Builder z Int
freeXORHelper in0 in1 o = Builder $ do
  context <- ask
  _       <- liftIO $ readIORef (gateIdx context)
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  -- generate output wire C_0 = A⊕B, C_1 = A⊕B⊕R
  --   where A, B := False, R := offset
  wire <- case o of
    Nothing -> do
      wIdx <- liftIO $ readIORef (wireIdx context)
      liftIO $ modifyIORef (wireIdx context) (+ 1)
      return wIdx
    (Just widx) -> return widx
  maskMap <- liftIO $ readIORef (wireMap context)
  let (a0, _) = case M.lookup in0 maskMap of
        Nothing ->
          error $ "freexor: input wire" ++ show in0 ++ "does not exist"
        (Just in0') -> in0'
      (b0, _) = case M.lookup in1 maskMap of
        Nothing ->
          error $ "freexor: input wire" ++ show in1 ++ "does not exist"
        (Just in1') -> in1'
      offset    = freeOffset context
      c@(c0, _) = (a0 `xorKey` b0, c0 `xorKey` offset)
      gate      = Free (in0, in1) wire
  liftIO $ modifyIORef (wireMap context) (M.insert wire c)
  unless (isRemote context) $ liftIO $ modifyIORef (circuit context)
                                                   (Seq.|> gate)
  runBuilder $ sendGate gate
  return wire

freeXOR :: Int -> Int -> Builder z Int
freeXOR a b = freeXORHelper a b Nothing

mkConstBit :: Bool -> Builder z Int
mkConstBit b = Builder $ do
  context <- ask
  liftIO $ modifyIORef (gateIdx context) (+ 1)
  wmap <- liftIO $ readIORef (wireMap context)
  gate <- case M.lookup (if b then -1 else -2) wmap of
    Nothing -> do
      offset  <- asks freeOffset
      (lo, _) <- liftIO $ genAESKeyPair128With offset
      liftIO $ modifyIORef (wireMap context)
                           (M.insert (if b then -1 else -2) (lo, lo))
      return $ Const (if b then -1 else -2) lo
    (Just (lo, _)) -> return $ Const (if b then -1 else -2) lo
  unless (isRemote context) $ liftIO $ modifyIORef (circuit context)
                                                   (Seq.|> gate)
  runBuilder $ sendGate gate
  return 0

sendGate :: Gate -> (forall z . Builder z ())
sendGate gate = Builder $ do
  remote <- asks isRemote
  when remote $ do
    sock <- fromJust <$> asks zmqSocket
    void . lift $ receive sock  -- just ignore whatever client says
    lift $ send sock [] (LBS.toStrict . encode $ gate) -- and send more gates!

