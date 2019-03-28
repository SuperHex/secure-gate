{-# LANGUAGE RankNTypes #-}

module Circuit.Gates where

import Circuit
import Circuit.Wire
import Control.Monad

type Index = Int

positive :: Index -> Builder Index
positive i = do
  n <- notGate i
  mkGate OR i n

negative :: Index -> Builder Index
negative i = mkGate XOR i i

constant :: Int -> Index -> Builder [Index]
constant n i =
  mapM (\b -> if b then positive i else negative i) (finiteToBits n)

halfAdder :: Index -> Index -> Builder (Index, Index)
halfAdder w0 w1 = do
  o0 <- mkGate XOR w0 w1
  o1 <- mkGate AND w0 w1
  return (o0, o1)

fullAdder :: Index -> Index -> Index -> Builder (Index, Index)
fullAdder a b c = do
  (s0, c0) <- halfAdder a b
  (s1, c1) <- halfAdder s0 c
  out      <- mkGate OR c0 c1
  return (s1, out)

adderN :: [Index] -> [Index] -> Index -> Builder [Index]
adderN []       []       _     = return []
adderN (x : xs) (y : ys) carry = do
  (s0, c0) <- fullAdder x y carry
  out      <- adderN xs ys c0
  return (s0 : out)

adder :: [Index] -> [Index] -> Builder [Index]
adder [] [] = return []
adder x y | length x /= length y = error "input wire lists' length differ!"
adder (x : xs) (y : ys) = do
  (s, c) <- halfAdder x y
  sums   <- adderN xs ys c
  return (s : sums)

(<+>) = adder

halfSubtractor :: Index -> Index -> Builder (Index, Index)
halfSubtractor x y = do
  o0 <- mkGate XOR x y
  o1 <- notGate x
  o2 <- mkGate AND y o1
  return (o0, o2)

fullSubtractor :: Index -> Index -> Index -> Builder (Index, Index)
fullSubtractor x y bin = do
  (d   , b ) <- halfSubtractor x y
  (diff, b') <- halfSubtractor d bin
  bout       <- mkGate OR b b'
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
  mkGate OR t f

ifThenElse :: Index -> [Index] -> [Index] -> Builder [Index]
ifThenElse s a b = sequenceA (zipWith (mux s) a b)

-- length (Builder [Index]) := 3
compareBit :: Index -> Index -> Builder (Index, Index, Index)
compareBit a b = do
  na  <- notGate a
  nb  <- notGate b
  l   <- mkGate AND na b
  g   <- mkGate AND nb a
  neq <- mkGate OR l g
  e   <- notGate neq
  return (l, e, g)

comparator :: [Index] -> [Index] -> Builder [Index]
comparator []       []       = return []
comparator (x : xs) (y : ys) = do
  (l, e, g) <- compareBit x y
  res       <- comparator xs ys
  gt        <- case res of
    [] -> ifThenElse g [l, e, g] [l, e, g]
    r  -> ifThenElse g [l, e, g] r
  ifThenElse l [l, e, g] gt

lt :: [Index] -> [Index] -> Builder Index
lt a b = fmap head (comparator a b)

le :: [Index] -> [Index] -> Builder Index
le a b = do
  c <- comparator a b
  mkGate OR (head c) (c !! 1)

gt :: [Index] -> [Index] -> Builder Index
gt a b = fmap (!! 2) (comparator a b)

ge :: [Index] -> [Index] -> Builder Index
ge a b = do
  c <- comparator a b
  mkGate OR (c !! 1) (c !! 2)

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
    ad  <- adder (tail x) (init xs)
    res <- go $ (ad ++ [last xs]) : ys
    return (head x : res)
