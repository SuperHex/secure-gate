{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils
  ( BasicHashMap
  , BasicHashT
  , newBasicHash
  , insertHash
  , lookupHash
  ) where

import qualified Data.HashTable.ST.Basic as Basic
import qualified Data.HashTable.IO as H

type BasicHashMap k v = H.IOHashTable Basic.HashTable k v

type BasicHashT = Basic.HashTable

newBasicHash = H.new @Basic.HashTable

insertHash = H.insert

lookupHash = H.lookup
