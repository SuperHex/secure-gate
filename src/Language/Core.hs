{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Core
  ( Id (..)
  , Pretty (..)
  , Core (..)
  , Fold (..)
  ) where

import Data.Bits
import Data.Kind (Type)
import Prelude   hiding ((+), (-), (*), (<=))


newtype Id repr a = Id { unId :: repr a }

newtype Pretty a = P { unP :: String }
  deriving Show

pretty = unP

class Core (repr :: Type -> Type) where
  lit :: Int -> repr Int
  (+) :: repr a -> repr a -> repr a
  (-) :: repr a -> repr a -> repr a
  (*) :: repr a -> repr a -> repr a
  lam :: (repr a -> repr b) -> repr (a -> b)
  lam2 :: (repr a -> repr b -> repr c) -> repr (a -> b -> c)
  app :: repr (a -> b) -> repr a -> repr b
  if_ :: repr Bool -> repr a -> repr a -> repr a
  (<=) :: repr b -> repr b -> repr Bool


class (Core repr) => Fold (repr :: Type -> Type) where
  foldl :: repr (b -> a -> b) -> repr b -> [repr a] -> repr b
  foldr :: repr (a -> b -> b) -> repr b -> [repr a] -> repr b
  scanl :: repr (b -> a -> b) -> repr b -> [repr a] -> [repr b]


instance Core repr => Core (Id repr) where
  lit n = Id $ lit n
  a + b = Id $ unId a + unId b
  a - b = Id $ unId a - unId b
  a * b = Id $ unId a * unId b
  lam f = Id $ lam (unId . f . Id)
  lam2 f  = Id $ lam2 (\a b -> unId $ f (Id a) (Id b))
  app f a = Id $ app (unId f) (unId a)
  if_ p a b = Id $ if_ (unId p) (unId a) (unId b)
  a <= b = Id $ unId a <= unId b

instance Core Pretty where
  lit n = P $ show n
  a + b = P $ unP a ++ " + " ++ unP b
  a - b = P $ unP a ++ " - " ++ unP b
  a * b = P $ unP a ++ " * " ++ unP b
  lam f = P $ "(\\x -> " ++ unP (f (P "x")) ++ ")"
  lam2 f = P $ "(\\x y -> " ++ unP (f (P "x") (P "y")) ++ ")"
  app f a = P $ "(" ++ unP f ++ " " ++ unP a ++ ")"
  if_ p a b = P $ "if " ++ unP p ++ " then " ++ unP a ++ " else " ++ unP b
  a <= b = P $ unP a ++ " <= "  ++ unP b
