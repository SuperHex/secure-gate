{-# LANGUAGE RankNTypes #-}

module Circuit.Gates where

import           Circuit
import           Circuit.Wire
import qualified Data.ByteString as BS
import Control.Monad.Reader

-- Note: All input bits are ordered as
--         LSB ------> MSB

type Index = Int

mkConst :: BS.ByteString -> Builder [Int]
mkConst s = traverse mkConstBit (toBits $ BS.unpack s)

notGate :: Int -> Builder Int
notGate x = do
  k <- mkConstBit True
  freeXOR k x

orGate :: Int -> Int -> Builder Int
orGate a b = do
  x  <- freeXOR a b
  an <- mkGate AND a b
  freeXOR x an

halfAdder :: Index -> Index -> Builder (Index, Index)
halfAdder w0 w1 = do
  o0 <- freeXOR w0 w1
  o1 <- mkGate AND w0 w1
  return (o0, o1)

fullAdder :: Index -> Index -> Index -> Builder (Index, Index)
fullAdder a b c = do
  (s0, c0) <- halfAdder a b
  (s1, c1) <- halfAdder s0 c
  out      <- orGate c0 c1
  return (s1, out)

adderN :: [Index] -> [Index] -> Index -> Builder ([Index], Index)
adderN []       []       c     = return ([], c)
adderN (x : xs) (y : ys) carry = do
  (s0 , c0) <- fullAdder x y carry
  (out, c ) <- adderN xs ys c0
  return (s0 : out, c)

adder :: [Index] -> [Index] -> Builder [Index]
adder [] [] = return []
adder x y | length x /= length y = error "input wire lists' length differ!"
adder (x : xs) (y : ys) = do
  (s   , c) <- halfAdder x y
  (sums, _) <- adderN xs ys c
  return (s : sums)

adderC (x : xs) (y : ys) = do
  (s , c ) <- halfAdder x y
  (s1, c1) <- adderN xs ys c
  return (s : s1, c1)

(<+>) = adder

halfSubtractor :: Index -> Index -> Builder (Index, Index)
halfSubtractor x y = do
  o0 <- freeXOR x y
  o1 <- notGate x
  o2 <- mkGate AND y o1
  return (o0, o2)

fullSubtractor :: Index -> Index -> Index -> Builder (Index, Index)
fullSubtractor x y bin = do
  (d   , b ) <- halfSubtractor x y
  (diff, b') <- halfSubtractor d bin
  bout       <- orGate b b'
  return (diff, bout)

subtractorN :: [Index] -> [Index] -> Index -> Builder [Index]
subtractorN []       []       _      = return []
subtractorN (x : xs) (y : ys) borrow = do
  (diff, bout) <- fullSubtractor x y borrow
  out          <- subtractorN xs ys bout
  return (diff : out)

subtractor :: [Index] -> [Index] -> Builder [Index]
subtractor [] [] = return []
subtractor x y | length x /= length y = error "input wire lists' length differ!"
subtractor (x : xs) (y : ys) = do
  (d, b) <- halfSubtractor x y
  diffs  <- subtractorN xs ys b
  return (d : diffs)

(<->) = subtractor

mux :: Index -> Index -> Index -> Builder Index
mux s a b = do
  n <- notGate s
  t <- mkGate AND s a
  f <- mkGate AND n b
  orGate t f

ifThenElse :: Index -> [Index] -> [Index] -> Builder [Index]
ifThenElse s a b = sequenceA (zipWith (mux s) a b)

-- length (Builder [Index]) := 3
compareBit :: Index -> Index -> Builder (Index, Index, Index)
compareBit a b = do
  na  <- notGate a
  nb  <- notGate b
  l   <- mkGate AND na b
  g   <- mkGate AND nb a
  neq <- orGate l g
  e   <- notGate neq
  return (l, e, g)

comparator :: [Index] -> [Index] -> Builder [Index]
comparator m n = go (reverse m) (reverse n)
 where
  go []       []       = return []
  go (x : xs) (y : ys) = do
    (l, e, g) <- compareBit x y
    res       <- go xs ys
    gt        <- case res of
      [] -> pure [l, e, g]
      r  -> ifThenElse g [l, e, g] r
    ifThenElse l [l, e, g] gt

lt :: [Index] -> [Index] -> Builder Index
lt a b = fmap head (comparator a b)

le :: [Index] -> [Index] -> Builder Index
le a b = do
  c <- comparator a b
  orGate (head c) (c !! 1)

gt :: [Index] -> [Index] -> Builder Index
gt a b = fmap (!! 2) (comparator a b)

ge :: [Index] -> [Index] -> Builder Index
ge a b = do
  c <- comparator a b
  orGate (c !! 1) (c !! 2)

eq :: [Index] -> [Index] -> Builder Index
eq a b = fmap (!! 1) (comparator a b)

basicMul :: [Index] -> [Index] -> Builder [Index]
basicMul a b = do
  let partial y x = mapM (mkGate AND x) y
      ps =
        if length a < length b then mapM (partial b) a else mapM (partial a) b
  interm <- ps
  go interm
 where
  go []            = return []
  go (x      : []) = return x
  go (x : xs : ys) = do
    (ad, c ) <- adderC (tail x) (init xs)
    (s , c') <- halfAdder c (last xs)
    res      <- goo ((ad ++ [s]) : ys) c'
    return (head x : res)
   where
    goo []            c     = return [c]
    goo (x      : []) c     = return (x ++ [c])
    goo (x : xs : ys) carry = do
      (ad, c ) <- adderC (tail x) (init xs)
      (s , c') <- fullAdder c (last xs) carry
      res      <- goo ((ad ++ [s]) : ys) c'
      return (head x : res)

