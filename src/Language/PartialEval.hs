{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PartialEval where

import Language.Core
import Prelude hiding (foldl, foldr, scanl, (+), (-), (*), (<=))

newtype UnFold repr a = UnFold { run :: repr a }
  deriving Core via (Id repr)

instance (Core repr) => Fold (UnFold repr) where
  foldl _ acc [] = acc
  foldl f acc (x : xs) = foldl f (app (app f acc) x) xs
  foldr _ acc [] = acc
  foldr f acc (x : xs) = app (app f x) (foldr f acc xs)
  scanl _  _  [] = []
  scanl f acc (x : xs) =
    let res = app (app f acc) x
    in  res : scanl f res xs

unFold :: (Core repr) => UnFold repr a -> repr a
unFold = run
