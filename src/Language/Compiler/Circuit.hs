{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Compiler.Circuit where

import           Circuit
import           Circuit.Gates
import           Circuit.Wire
import           Control.Monad   (liftM2)
import qualified Data.ByteString as BS
import           Data.Fix
import qualified Data.Function   as F (fix)
import           Language.Core
import           Prelude         hiding (pred)

data W a = Bit Bool | Lit Int [Bool] | Wires [Int] | Func a
  deriving Show

newtype C st a = C { circ :: [Int] -> [Int] -> Builder [Int] }

runWith :: [Int] -> [Int] -> C st a -> Builder [Int]
runWith x y c = circ c x y

const2 :: a -> b -> c -> a
const2 a _ _ = a

instance Core C where
  int n = C $ const2 $ mkConst (BS.pack . fromBits . finiteToBits $ n)
  bool b = C $ const2 $ mkConstBit b >>= \a -> return [a]
  a .+ b = C $ \i0 i1 -> do
    a' <- circ a i0 i1
    b' <- circ b i1 i0
    adder a' b'
  a .- b = C $ \i0 i1 -> do
    a' <- circ a i0 i1
    b' <- circ b i1 i0
    subtractor a' b'
  lam f = C $ \i0 i1 -> circ (f (C $ const2 $ pure i0)) i1 i0
  app f a = C $ \x y -> do
    k <- circ a x y
    circ f k y
  if_ p a b = C $ \i0 i1 -> do
    pred <- circ p i0 i1
    case pred of
      [] -> fail "ifThenElse error: no input wires"
      (pr : _) -> do
        x <- circ a i0 i1
        y <- circ b i0 i1
        ifThenElse pr x y
  a .<= b = C $ \i0 i1 -> do
    a' <- circ a i0 i1
    b' <- circ b i1 i0
    le a' b' >>= \x -> pure [x]
  fix f =
    let self e = app (f (lam self)) e
    in C $ \i0 i1 -> circ (self (C $ const2 $ pure i0)) i1 i0


data F st a = Fuck { st :: Maybe st, dy :: Fix B }
data B f = F (Builder [Int] -> f) | B (Builder [Int])

instance Core F where
  int n = Fuck { st = Just n, dy = Fix $ B $ mkConst (BS.pack . fromBits . finiteToBits $ n) }
  lam f = Fuck { st = Just f, dy = Fix $ F (\w -> dy (f (Fuck { st = Nothing, dy = Fix $ B w}))) }
  app f a = case st f of
    Just f' -> f' a
    Nothing -> case (dy f, dy a) of
      (Fix (F f''), Fix (B b)) -> Fuck { st = Nothing, dy = f'' b }
      (Fix (F f''), Fix (F g)) -> Fuck { st = Nothing, dy = undefined }

instance Functor B where
  fmap f (B b) = B b
  fmap f (F ff) = F $ \x -> f (ff x)
