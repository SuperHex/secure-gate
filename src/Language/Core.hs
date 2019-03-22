{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Core
  ( Id (..)
  , Pretty (..)
  , Core (..)
  ) where

import Data.Bits
import Data.Kind (Type)
import Data.Word
import qualified Data.Function as F

newtype Id repr st a = Id { unId :: repr st a }

newtype Pretty st a = P { unP :: Int -> String }

pretty = flip unP 0

class Core (repr :: Type -> Type -> Type) where
  int  :: Int -> repr st Int
  bool :: Bool -> repr st Bool
  (.+) :: (Num a) => repr st a -> repr st a -> repr st a
  (.-) :: (Num a) => repr st a -> repr st a -> repr st a
  (.*) :: (Num a) => repr st a -> repr st a -> repr st a
  lam :: (repr st a -> repr st b) -> repr st (a -> b)
  app :: repr st (a -> b) -> repr st a -> repr st b
  fix :: (repr st a -> repr st a) -> repr st a
  if_ :: repr st Bool -> repr st a -> repr st a -> repr st a
  (.<=) :: (Ord b) => repr st b -> repr st b -> repr st Bool

lam2
  :: (Core repr)
  => (repr st a -> repr st b -> repr st c)
  -> repr st (a -> b -> c)
lam2 f = lam (lam . f)

app2 f a = app (app f a)

-- class Hardware (repr :: Type -> Type) where
--   high :: repr Bool
--   low :: repr Bool
--   (.&&.) :: repr [Bool] -> repr [Bool] -> repr [Bool]
--   batch :: [repr Bool] -> repr [Bool]

-- newtype H repr a = H { unH :: repr [Bool] }

-- instance (Hardware repr) => Core (H repr) where
--   int n = H $ batch (replicate 10 high)
--   a .+ b = H $ unH a .&&. unH b

-- instance Hardware Pretty where
--   high = P $ \t -> "h" ++ show t
--   low = P $ \t -> "l" ++ show t
--   a .&&. b = P $ \t -> unP a t ++ " && " ++ unP b (t + 1)
--   batch x = P $ \t -> "wire[" ++ show t ++ "-" ++ show (length x + t) ++ "]"

instance Core repr => Core (Id repr) where
  int n = Id $ int n
  bool b = Id $ bool b
  a .+ b = Id $ unId a .+ unId b
  a .- b = Id $ unId a .- unId b
  a .* b = Id $ unId a .* unId b
  lam f = Id $ lam (unId . f . Id)
  app f a = Id $ app (unId f) (unId a)
  fix f = Id $ fix (unId . f . Id)
  if_ p a b = Id $ if_ (unId p) (unId a) (unId b)
  a .<= b = Id $ unId a .<= unId b

instance Core Pretty where
  int n = P $ const (show n)
  bool b = P $ const (show b)
  a .+ b = P $ \c -> unP a c ++ " + " ++ unP b c
  a .- b = P $ \c -> unP a c ++ " - " ++ unP b c
  a .* b = P $ \c -> unP a c ++ " * " ++ unP b c
  lam f = P $ \c -> "(\\x" ++ show c ++ " -> " ++ unP (f (P . const $ "x" ++ show c)) (c + 1) ++ ")"
  app f a = P $ \c -> "(" ++ unP f c ++ " " ++ unP a c ++ ")"
  fix f = P $ \c -> "fix " ++ unP (lam f) c
  if_ p a b = P $ \c -> "if " ++ unP p c ++ " then " ++ unP a c ++ " else " ++ unP b c
  a .<= b = P $ \c -> unP a c ++ " <= "  ++ unP b c

newtype Eval st a = Eval { unEval :: a }

instance Core Eval where
  int = Eval
  bool = Eval
  a .+ b = Eval $ unEval a + unEval b
  a .- b = Eval $ unEval a - unEval b
  a .* b = Eval $ unEval a * unEval b
  lam f = Eval $ unEval . f . Eval
  app f a = Eval $ unEval f (unEval a)
  fix f = Eval $ F.fix (unEval (lam f))
  if_ p a b = Eval $ if unEval p then unEval a else unEval b
  a .<= b = Eval $ unEval a <= unEval b

