{-# LANGUAGE RankNTypes #-}

module Circuit.Wire where

import Circuit (mkWire, Builder)
import Control.Monad (replicateM)

{-# INLINE in8 #-}
in8 :: Builder [Int]
in8 = replicateM 8 mkWire

{-# INLINE in16 #-}
in16 :: Builder [Int]
in16 = replicateM 16 mkWire

{-# INLINE in32 #-}
in32 :: Builder [Int]
in32 = replicateM 32 mkWire

{-# INLINE in64 #-}
in64 :: Builder [Int]
in64 = replicateM 64 mkWire

{-# INLINE inN #-}
inN :: Int -> Builder [Int]
inN n = replicateM n mkWire
