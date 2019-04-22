{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Compiler.Circuit where

import           Circuit.Class
import           Circuit.Gates
import           Control.Monad        (liftM2)
import qualified Control.Monad.Fail   as Fail
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString      as BS
import qualified Data.Function        as F (fix)
import qualified Data.Map             as M
import           Data.Word
import           Language.Core
import           Language.Extension
import           Prelude              hiding (pred)
import           System.ZMQ4.Monadic
import           Utils

instance Fail.MonadFail (ZMQ z) where
  fail = error

data W a = Bit Bool | Lit Int [Bool] | Wires [Int] | Func a
  deriving Show

newtype C z st a = C { circ :: [Int] -> [Int] -> Builder z V }

runWith :: [Int] -> [Int] -> (forall z . C z st a -> Builder z [Int])
runWith x y c = flip fmap (circ c x y) $ \case
  (L a  ) -> a
  (V _ a) -> a

const2 :: a -> b -> c -> a
const2 a _ _ = a

instance Core (C z) where
  word8 n = C $ const2 $ mkConst (BS.pack . bitsToBytes . fromFinite $ n)
  int n = C $ const2 $ mkConst (BS.pack . bitsToBytes . fromFinite $ n)
  bool b = C $ const2 $ mkConstBit b >>= \a -> return [a]
  a .+ b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    L <$> adder a' b'
  a .- b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    L <$> subtractor a' b'
  a .* b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    L <$> basicMul a' b'
  lam f = C $ \i0 i1 -> circ (f (C $ const2 $ pure (L i0))) i1 i1
  app f a = C $ \x y -> do
    (L k) <- circ a x y
    circ f x k
  if_ p a b = C $ \i0 i1 -> do
    pred <- circ p i0 i1
    case pred of
      (L []) -> fail "ifThenElse error: no input wires"
      (L (pr : _)) -> do
        (L x) <- circ a i0 i1
        (L y) <- circ b i0 i1
        L <$> ifThenElse pr x y
      _ -> fail "if not support list"
  a .<= b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    fmap L $ le a' b' >>= \x -> pure [x]
  a .== b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    fmap L $ eq a' b' >>= \x -> pure [x]
  a .!= b = C $ \i0 i1 -> do
    (L a') <- circ a i0 i0
    (L b') <- circ b i1 i1
    e  <- eq a' b'
    fmap L $ notGate e >>= \x -> pure [x]

data V = V Int [Int] | L [Int]
  deriving Show

instance List C where
  arr [] = C $ const2 $ pure (V 0 [])
  arr xs = C $ \a b -> do
    list <- traverse (\x -> circ x a b) xs
    pure $ V (length list) (concatMap (\(L x) -> x) list)
  hd xs = C $ \a b -> do
    (V n v) <- circ xs a b
    pure . L $ take (length v `div` n) v
  tl xs = C $ \a b -> do
    (V n v) <- circ xs a b
    pure . V (n - 1) $ drop (length v `div` n) v

instance Scan C where
  scan f x xs = C $ \a b -> do
    vec@(V n v) <- circ xs a b
    case n of
      0 -> pure vec
      _ -> do
        let list = chunksOf (length v `div` n) v
        (V _ acc) <- circ x a b
        undefined

-- data F st a = Fuck { st :: Maybe st, dy :: Fix B }
-- data B f = F (Builder [Int] -> f) | B (Builder [Int])

-- instance Core F where
--   int n = Fuck { st = Just n, dy = Fix $ B $ mkConst (BS.pack . bitsToBytes . fromBits $ n) }
--   lam f = Fuck { st = Just f, dy = Fix $ F (\w -> dy (f (Fuck { st = Nothing, dy = Fix $ B w}))) }
--   app f a = case st f of
--     Just f' -> f' a
--     Nothing -> case (dy f, dy a) of
--       (Fix (F f''), Fix (B b)) -> Fuck { st = Nothing, dy = f'' b }
--       (Fix (F f''), Fix (F g)) -> Fuck { st = Nothing, dy = undefined }

-- instance Functor B where
--   fmap f (B b) = B b
--   fmap f (F ff) = F $ \x -> f (ff x)
