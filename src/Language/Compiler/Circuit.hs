{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Language.Compiler.Circuit where

import           Circuit.Class
import           Circuit.Gates
import           Control.Monad        (liftM2)
import qualified Control.Monad.Fail   as Fail
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString      as BS
import qualified Data.Function        as F (fix)
import qualified Data.Map             as M
import           Data.Word
import           Language.Core
import           Language.Extension
import           Language.QuasiQuote
import           Prelude              hiding (pred)
import           System.ZMQ4.Monadic
import           Utils

instance Fail.MonadFail (Builder z) where
  fail = error

data W a = Bit Bool | Lit Int [Bool] | Wires [Int] | Func a
  deriving Show

newtype C z st a = C { circ :: [Int] -> [Int] -> Builder z V }

runWith :: [Int] -> [Int] -> (forall z . C z st a -> Builder z [Int])
runWith x y c = fmap flatV (circ c x y)

const2 :: a -> b -> c -> a
const2 a _ _ = a

instance Core (C z) where
  word8 n = C . const2 $ L <$> mkConst (BS.pack . bitsToBytes . fromFinite $ n)
  int n = C . const2 $ L <$> mkConst (BS.pack . bitsToBytes . fromFinite $ n)
  bool b = C . const2 . fmap L $ mkConstBit b >>= \a -> return [a]
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
  fix _ = undefined


-- sc :: (Monad m) => (b -> a -> m b) -> m b -> [m a] -> m [m b]
-- sc _ _   []       = pure []
-- sc f acc (x : xs) = do
--   acc' <- acc
--   x'   <- x
--   f'   <- f acc' x'
--   (pure f' :) <$> sc f (pure f') xs

data V = V Int [Int] | L [Int]
  deriving Show

flatV :: V -> [Int]
flatV = \case
  L x     -> x
  (V _ x) -> x

instance List (C z) where
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

instance Scan (C z) where
  scan f x xs = C $ \a b -> do
    vec@(V n v) <- circ xs a b
    case n of
      0 -> pure vec
      _ -> do
        let list = chunksOf (length v `div` n) v
            (C g) = f
        (V _ acc) <- circ x a b
        V n . concat <$> scanlM (\p q -> flatV <$> g p q) acc list

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


data CompileState = CS

type Compiler z a = RWST Int CompileState Int (Builder z) a

secureRun = unView . compile

compile :: Expr -> Builder z (Int :~> String)
compile (Int64 n ) = view $ mkConst (BS.pack . bitsToBytes . fromFinite $ n)
compile (Input i ) = pure i
compile (e1 :+ e2) = do
  a1 <- compile e1
  a2 <- compile e2
  case (a1, a2) of
    (VInt64 n1   , VInt64 n2   ) -> view (adder n1 n2)
    (VString _ s1, VString _ s2) -> view (adder s1 s2)
compile (e1 :- e2) = do
  a1 <- unView $ compile e1
  a2 <- unView $ compile e2
  view $ subtractor a1 a2
