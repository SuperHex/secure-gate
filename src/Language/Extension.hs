{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Extension where

import Language.Core
import Data.Word
import GHC.TypeLits
import Circuit.Class
import Data.Proxy
import Data.Coerce

minimum_ :: (Core repr, Ord st, Ord b) => [repr st b] -> repr st b
minimum_ = foldr1 (\a b -> if_ (a .<= b) a b)

minimumA :: (Fold repr, Ord st, Ord b) => repr st [b] -> repr st b
minimumA xs = rFold (lam2 $ \a b -> if_ (a .<= b) a b) (hd xs) (tl xs)

maximumA :: (Fold repr, Ord st, Ord b) => repr st [b] -> repr st b
maximumA xs = rFold (lam2 $ \a b -> if_ (a .<= b) b a) (hd xs) (tl xs)

leven
  :: (Core repr, Eq st)
  => [repr st Word8]
  -> [repr st Word8]
  -> repr Word8 Word8
leven s1 s2 = last
  $ foldl transform (fmap word8 [0 .. fromIntegral $ length s1]) s2
 where
   -- TODO: share intermidiate value in scanl
  transform ns@(n : ns1) c = scanl calc (n .+ word8 1) $ zip3 s1 ns ns1
   where
    calc z (c1, x, y) = minimum_
      [y .+ word8 1, z .+ word8 1, x .+ if_ (c1 .!= c) (word8 1) (word8 0)]

scanlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM _ _ []       = pure []
scanlM f i (x : xs) = do
  a <- f i x
  (a :) <$> scanlM f a xs

class (Core repr) => List repr where
  arr :: forall st (a :: *) . [repr st a] -> repr st [a]
  hd :: repr st [a] -> repr st a
  tl :: repr st [a] -> repr st [a]

class (Core repr, List repr) => Scan repr where
  scan :: repr (repr st1 b -> repr (repr st2 a -> repr st3 b) (a -> b)) (b -> a -> b) -> repr st1 b -> repr st2 [a] -> repr st3 [b]

class (Core repr, List repr) => Fold repr where
  lFold :: repr (repr st1 b -> repr (repr st2 a -> repr st3 b) (a -> b)) (b -> a -> b) -> repr st1 b -> repr st2 [a] -> repr st1 b
  rFold :: repr (repr st1 a -> repr (repr st2 b -> repr st3 b) (a -> b)) (a -> b -> b) -> repr st1 b -> repr st2 [a] -> repr st1 b

data Var a

class (Core repr) => Variable repr where
  intVar :: String -> repr (Var Int) (Var Int)
  readVar :: repr (Var a) (Var a) -> repr a a
  writeVar :: repr (Var a) (Var a) -> repr a a -> repr () ()
