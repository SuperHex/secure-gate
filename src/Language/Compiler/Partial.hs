{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Language.Compiler.Partial where

import Language.Compiler.Meta
import Language.Core
import Language.Haskell.TH hiding (dyn)
import Data.Coerce

type role P phantom nominal phantom
data P st' st a = P
  { stt :: Maybe st
  , dyn :: T st' a
  }

instance (Num st', Eq st', Ord st') => Core (P st') where
  int n = P { stt = Just n, dyn = coerce (int n :: T Int Int) :: T st' Int }
  bool b = P { stt = Just b, dyn = coerce (bool b :: T Bool Bool) :: T st' Bool }
  a .+ b = case (stt a, stt b) of
    (Just 0, Just m) -> b { stt = Just m }
    (Just n, Just 0) -> a { stt = Just n }
    (Just n, Just m) -> P { stt = Just $ m + n, dyn = dyn a .+ dyn b }
    _ -> P { stt = Nothing, dyn = dyn a .+ dyn b}
  a .- b = case (stt a, stt b) of
    (Just n, Just 0) -> a { stt = Just n }
    (Just n, Just m) -> P { stt = Just $ n - m, dyn = dyn a .- dyn b }
    _ -> P { stt = Nothing, dyn = dyn a .- dyn b }
  a .* b = case (stt a, stt b) of
    (Just 0, Just _) -> a
    (Just 1, Just _) -> b
    (Just _, Just 0) -> b
    (Just _, Just 1) -> a
    (Just n, Just m) -> P { stt = Just (n * m), dyn = dyn a .* dyn b }
    _ -> P { stt = Nothing, dyn = dyn a .* dyn b }
  lam f = P { stt = Just f, dyn = coerce $ lam (\a -> dyn (f P {stt = Nothing, dyn = a})) }
  app f a = case stt f of
    Just f' -> f' a
    Nothing -> P { stt = Nothing, dyn = app (coerce $ dyn f) (dyn a) }
  fix f =
    let self e@(P (Just _) _) = app (f (lam self)) e
        self e = P { stt = Nothing, dyn = app (coerce dynf) (dyn e) }
    in P { stt = Just self, dyn = coerce dynf }
    where
      dynf :: T (T sa a -> T sb b) (a -> b)
      dynf = fix (\a -> coerce $ dyn (f P { stt = Nothing, dyn = coerce a}))
  if_ p a b = case stt p of
    Just p' -> if p' then a else b
    Nothing -> P { stt = Nothing, dyn = if_ (coerce $ dyn p) (dyn a) (dyn b) }
  a .<= b = case (stt a, stt b) of
    (Just n, Just m) -> P { stt = Just (n <= m), dyn = coerce $ dyn a .<= dyn b }
    _ -> P { stt = Nothing, dyn = coerce $ dyn a .<= dyn b }
