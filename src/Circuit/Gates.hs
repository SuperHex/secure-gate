{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Circuit.Gates where

import           Circuit.Class
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import           Data.List.Split
import           Prelude              hiding (and)
import           Utils                (bytesToBits)

-- Note: All input bits are ordered as
--         LSB ------> MSB

type Index = Int

mkConst :: (Component c a) => BS.ByteString -> c [a]
mkConst s =
  traverse (\x -> if x then high else low) (bytesToBits $ BS.unpack s)

notGate :: (Component c a) => a -> c a
notGate x = do
  k <- high
  k `xor` x

notGate' :: Int -> Int -> Builder z Int
notGate' i o = do
  k <- high
  freeXORHelper k i (Just o)

orGate :: (Component c a) => a -> a -> c a
orGate a b = do
  x  <- xor a b
  an <- and a b
  x `xor` an

norGate :: (Component c a) => a -> a -> c a
norGate a b = orGate a b >>= notGate

nxorGate :: (Component c a) => a -> a -> c a
nxorGate a b = a `xor` b >>= notGate

halfAdder :: (Component c a) => a -> a -> c (a, a)
halfAdder w0 w1 = do
  o0 <- w0 `xor` w1
  o1 <- w0 `and` w1
  return (o0, o1)

fullAdder :: (Component c a) => a -> a -> a -> c (a, a)
fullAdder a b c = do
  (s0, c0) <- halfAdder a b
  (s1, c1) <- halfAdder s0 c
  out      <- orGate c0 c1
  return (s1, out)

adderN :: (Component c a) => [a] -> [a] -> a -> c ([a], a)
adderN []       []       c     = return ([], c)
adderN (x : xs) (y : ys) carry = do
  (s0 , c0) <- fullAdder x y carry
  (out, c ) <- adderN xs ys c0
  return (s0 : out, c)

adder :: (Component c a) => [a] -> [a] -> c [a]
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

halfSubtractor :: (Component c a) => a -> a -> c (a, a)
halfSubtractor x y = do
  o0 <- x `xor` y
  o1 <- notGate x
  o2 <- y `and` o1
  return (o0, o2)

fullSubtractor :: (Component c a) => a -> a -> a -> c (a, a)
fullSubtractor x y bin = do
  (d   , b ) <- halfSubtractor x y
  (diff, b') <- halfSubtractor d bin
  bout       <- orGate b b'
  return (diff, bout)

subtractorN :: (Component c a) => [a] -> [a] -> a -> c [a]
subtractorN []       []       _      = return []
subtractorN (x : xs) (y : ys) borrow = do
  (diff, bout) <- fullSubtractor x y borrow
  out          <- subtractorN xs ys bout
  return (diff : out)

subtractor :: (Component c a) => [a] -> [a] -> c [a]
subtractor [] [] = return []
subtractor x y | length x /= length y = error "input wire lists' length differ!"
subtractor (x : xs) (y : ys) = do
  (d, b) <- halfSubtractor x y
  diffs  <- subtractorN xs ys b
  return (d : diffs)

mux :: (Component c a) => a -> a -> a -> c a
mux s a b = do
  n <- notGate s
  t <- s `and` a
  f <- n `and` b
  orGate t f

ifThenElse :: (Component c a) => a -> [a] -> [a] -> c [a]
ifThenElse s a b = sequenceA (zipWith (mux s) a b)

-- length (Builder [Index]) := 3
compareBit :: (Component c a) => a -> a -> c (a, a, a)
compareBit a b = do
  na  <- notGate a
  nb  <- notGate b
  l   <- na `and` b
  g   <- nb `and` a
  neq <- orGate l g
  e   <- notGate neq
  return (l, e, g)

comparator :: (Component c a) => [a] -> [a] -> c [a]
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

cmp4 :: (Component c a) => [a] -> [a] -> c (a, a, a)
cmp4 a b
  | length a /= 4 || length b /= 4
  = error "cmp4: input length not equal to 4"
  | otherwise
  = let (a3 : a2 : a1 : a0 : []) = a
        (b3 : b2 : b1 : b0 : []) = b
    in  do
          nb3      <- notGate b3
          nb2      <- notGate b2
          nb1      <- notGate b1
          nb0      <- notGate b0
          s5       <- nxorGate a3 b3
          s6       <- nxorGate a2 b2
          s7       <- nxorGate a1 b1
          s8       <- nxorGate a0 b0
          s1       <- a3 `and` nb3
          s2_0     <- a2 `and` nb2
          s2       <- s2_0 `and` s5
          s3_0     <- a1 `and` nb1
          s3_1     <- s5 `and` s6
          s3       <- s3_0 `and` s3_1
          s4_0     <- a0 `and` nb0
          s4_1     <- s5 `and` s6
          s4_2     <- s4_0 `and` s4_1
          s4       <- s4_2 `and` s7
          -- output A > B
          a_gt_b_0 <- orGate s1 s2
          a_gt_b_1 <- orGate s3 s4
          a_gt_b   <- orGate a_gt_b_0 a_gt_b_1
          -- output A = B
          a_eq_b_0 <- s5 `and` s6
          a_eq_b_1 <- s7 `and` s8
          a_eq_b   <- a_eq_b_0 `and` a_eq_b_1
          -- output A < B
          a_lt_b   <- norGate a_gt_b a_eq_b
          return (a_lt_b, a_eq_b, a_gt_b)

cmp4MSB :: (Component c a) => [a] -> [a] -> (a, a, a) -> c (a, a, a)
cmp4MSB a b (lin, ein, gin) = do
  (l, e, g) <- cmp4 a b
  -- A < B
  and1      <- l `and` ein
  altb      <- orGate and1 lin
  -- A = B
  aeqb      <- e `and` ein
  -- A > B
  and2      <- g `and` ein
  agtb      <- orGate and2 gin
  return (altb, aeqb, agtb)

cmp4N :: (Component c a) => [a] -> [a] -> c [a]
cmp4N a b =
  let (x : a') = chunksOf 4 (reverse a)
      (y : b') = chunksOf 4 (reverse b)
  in  do
        c         <- cmp4 x y
        (l, e, g) <- foldM (\m f -> f m) c $ zipWith cmp4MSB a' b'
        return [l, e, g]

eq4 :: (Component c a) => [a] -> [a] -> c a
eq4 a b
  | length a /= 4 || length b /= 4
  = error "eq4: input length not qeual to 4"
  | otherwise
  = let (a0 : a1 : a2 : a3 : []) = a
        (b0 : b1 : b2 : b3 : []) = b
    in  do
          o0   <- nxorGate a0 b0
          o1   <- nxorGate a1 b1
          o2   <- nxorGate a2 b2
          o3   <- nxorGate a3 b3
          and0 <- o0 `and` o1
          and1 <- o2 `and` o3
          and0 `and` and1

eq4N :: (Component c a) => [a] -> [a] -> c a
eq4N a b =
  let a' = chunksOf 4 a
      b' = chunksOf 4 b
  in  do
        r <- zipWithM eq4 a' b'
        case r of
          []       -> error "eq4N: empty input"
          (x : xs) -> foldM and x xs

lt :: (Component c a) => [a] -> [a] -> c a
lt a b = fmap head (cmp4N a b)

le :: (Component c a) => [a] -> [a] -> c a
le a b = do
  c <- cmp4N a b
  orGate (head c) (c !! 1)

gt :: (Component c a) => [a] -> [a] -> c a
gt a b = fmap (!! 2) (cmp4N a b)

ge :: (Component c a) => [a] -> [a] -> c a
ge a b = do
  c <- cmp4N a b
  orGate (c !! 1) (c !! 2)

eq :: (Component c a) => [a] -> [a] -> c a
eq = eq4N

basicMul :: (Component c a) => [a] -> [a] -> c [a]
basicMul a b = do
  let partial y x = mapM (x `and`) y
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

