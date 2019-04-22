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

module Language.Extension where

import Language.Core
import Data.Word
import GHC.TypeLits
import Circuit
import Data.Proxy


minimum_ :: (Core repr, Foldable t, Ord st, Ord b) => t (repr st b) -> repr st b
minimum_ = foldr1 (\a b -> if_ (a .<= b) a b)

maximum_ :: (Core repr, Foldable t, Ord st, Ord b) => t (repr st b) -> repr st b
maximum_ = foldr1 (\a b -> if_ (a .<= b) b a)

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

scanlM :: (Monad m) => (m b -> m a -> m b) -> m b -> [m a] -> m [b]
scanlM _ _ []       = pure []
scanlM f i (x : xs) = do
  a <- f i x
  (a :) <$> scanlM f (pure a) xs

class (Core repr) => List repr where
  arr :: forall st (a :: *) . [repr st a] -> repr st [a]
  hd :: repr st [a] -> repr st a
  tl :: repr st [a] -> repr st [a]

class (Core repr, List repr) => Scan repr where
  scan :: repr st (b -> a -> b) -> repr st b -> repr st [a] -> repr st [b]
