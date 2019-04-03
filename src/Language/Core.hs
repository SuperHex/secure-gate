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
  , Eval (..)
  , Pretty (..)
  , Core (..)
  , lam2, app2
  ) where

import Data.Bits
import Data.Kind (Type)
import Data.Word
import qualified Data.Function as F

newtype Id repr st a = Id { unId :: repr st a }

newtype Pretty st a = Pretty { unP :: Int -> String }

pretty = flip unP 0

class Core (repr :: Type -> Type -> Type) where
  word8 :: Word8 -> repr Word8 Word8
  int  :: Int -> repr Int Int
  bool :: Bool -> repr Bool Bool
  (.+) :: (Num a, Num st, Eq st) => repr st a -> repr st a -> repr st a
  (.-) :: (Num a, Num st, Eq st) => repr st a -> repr st a -> repr st a
  (.*) :: (Num a, Num st, Eq st) => repr st a -> repr st a -> repr st a
  lam :: (repr sta a -> repr stb b) -> repr (repr sta a -> repr stb b) (a -> b)
  app :: repr (repr sta a -> repr stb b) (a -> b) -> repr sta a -> repr stb b
  fix ::
      (repr (repr sa a -> repr sb b) (a -> b) -> repr (repr sa a -> repr sb b) (a -> b))
      -> repr (repr sa a -> repr sb b) (a -> b)
  if_ :: repr Bool Bool -> repr st a -> repr st a -> repr st a
  (.<=) :: (Ord b, Ord st) => repr st b -> repr st b -> repr Bool Bool
  (.==) :: (Eq b, Eq st) => repr st b -> repr st b -> repr Bool Bool
  (.!=) :: (Eq b, Eq st) => repr st b -> repr st b -> repr Bool Bool

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

-- instance Core repr => Core (Id repr) where
--   int n = Id $ int n
--   bool b = Id $ bool b
--   a .+ b = Id $ unId a .+ unId b
--   a .- b = Id $ unId a .- unId b
--   a .* b = Id $ unId a .* unId b
--   lam f = Id $ lam (unId . f . Id)
--   app f a = Id $ app (unId f) (unId a)
--   fix f = Id $ fix (unId . f . Id)
--   if_ p a b = Id $ if_ (unId p) (unId a) (unId b)
--   a .<= b = Id $ unId a .<= unId b

instance Core Pretty where
  word8 n = Pretty $ const (show n)
  int n = Pretty $ const (show n)
  bool b = Pretty $ const (show b)
  a .+ b = Pretty $ \c -> unP a c ++ " + " ++ unP b c
  a .- b = Pretty $ \c -> unP a c ++ " - " ++ unP b c
  a .* b = Pretty $ \c -> unP a c ++ " * " ++ unP b c
  lam f = Pretty $ \c -> "(\\x" ++ show c ++ " -> " ++ unP (f (Pretty . const $ "x" ++ show c)) (c + 1) ++ ")"
  app f a = Pretty $ \c -> "(" ++ unP f c ++ " " ++ unP a c ++ ")"
  fix f = Pretty $ \c -> "fix " ++ unP (lam f) c
  if_ p a b = Pretty $ \c -> "if " ++ unP p c ++ " then " ++ unP a c ++ " else " ++ unP b c
  a .<= b = Pretty $ \c -> unP a c ++ " <= "  ++ unP b c
  a .== b = Pretty $ \c -> unP a c ++ " == " ++ unP b c
  a .!= b = Pretty $ \c -> unP a c ++ " /= " ++ unP b c

newtype Eval st a = Eval { unEval :: a } deriving Show

instance Core Eval where
  word8 = Eval
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
  a .== b = Eval $ unEval a == unEval b
  a .!= b = Eval $ unEval a /= unEval b
