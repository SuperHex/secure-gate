{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Circuit.Gates where

import           Circuit.Class
import           Control.Monad.Reader
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.List.Split
import           Data.Word             (Word8)
import           Numeric               (showHex)
import           Prelude               hiding (and, minimum)
import           Utils                 (bytesToBits, foldlM, scanlM)

-- Note: All input bits are ordered as
--         LSB ------> MSB

type Index = Int

mkConst :: (Component c a) => BS.ByteString -> c [a]
mkConst s =
  traverse (\x -> if x then high else low) (bytesToBits $ BS.unpack s)

int :: (Component c a) => Int -> c [a]
int = mkConst . BS.pack . fmap (toEnum . subtract 48 . fromEnum) . show

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
  r  <- x `xor` an
  clearWires [x, an]
  return r

norGate :: (Component c a) => a -> a -> c a
norGate a b = do
  x <- orGate a b
  r <- notGate x
  clearWire x
  return r

nxorGate :: (Component c a) => a -> a -> c a
nxorGate a b = do
  x <- a `xor` b
  r <- notGate x
  clearWire x
  return r

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
  clearWires [s0, c0, c1]
  return (s1, out)

adderN :: (Component c a) => [a] -> [a] -> a -> c ([a], a)
adderN []       []       c     = return ([], c)
adderN (x : xs) (y : ys) carry = do
  (s0 , c0) <- fullAdder x y carry
  (out, c ) <- adderN xs ys c0
  clearWire c0
  return (s0 : out, c)

adder :: (Component c a) => [a] -> [a] -> c [a]
adder [] [] = return []
adder x y | length x /= length y = error "input wire lists' length differ!"
adder (x : xs) (y : ys) = do
  (s   , c ) <- halfAdder x y
  (sums, c1) <- adderN xs ys c
  clearWires [c, c1]
  return (s : sums)

adderC (x : xs) (y : ys) = do
  (s , c ) <- halfAdder x y
  (s1, c1) <- adderN xs ys c
  clearWire c
  return (s : s1, c1)

halfSubtractor :: (Component c a) => a -> a -> c (a, a)
halfSubtractor x y = do
  o0 <- x `xor` y
  o1 <- notGate x
  o2 <- y `and` o1
  clearWire o1
  return (o0, o2)

fullSubtractor :: (Component c a) => a -> a -> a -> c (a, a)
fullSubtractor x y bin = do
  (d   , b ) <- halfSubtractor x y
  (diff, b') <- halfSubtractor d bin
  bout       <- orGate b b'
  clearWires [d, b, b']
  return (diff, bout)

subtractorN :: (Component c a) => [a] -> [a] -> a -> c [a]
subtractorN []       []       _      = return []
subtractorN (x : xs) (y : ys) borrow = do
  (diff, bout) <- fullSubtractor x y borrow
  out          <- subtractorN xs ys bout
  clearWire bout
  return (diff : out)

subtractor :: (Component c a) => [a] -> [a] -> c [a]
subtractor [] [] = return []
subtractor x y | length x /= length y = error "input wire lists' length differ!"
subtractor (x : xs) (y : ys) = do
  (d, b) <- halfSubtractor x y
  diffs  <- subtractorN xs ys b
  clearWire b
  return (d : diffs)

mux :: (Component c a) => a -> a -> a -> c a
mux s a b = do
  n <- notGate s
  t <- s `and` a
  f <- n `and` b
  o <- orGate t f
  clearWires [n, t, f]
  return o

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
  clearWires [na, nb, neq]
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
  = let [a3, a2, a1, a0] = a
        [b3, b2, b1, b0] = b
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
          clearWires [s1, s2, s3, s4, a_gt_b_0, a_gt_b_1]
          -- output A = B
          a_eq_b_0 <- s5 `and` s6
          a_eq_b_1 <- s7 `and` s8
          a_eq_b   <- a_eq_b_0 `and` a_eq_b_1
          clearWires [s5, s6, s7, s8, a_eq_b_0, a_eq_b_1]
          -- output A < B
          a_lt_b <- norGate a_gt_b a_eq_b
          clearWires [nb3, nb2, nb1, nb0, s2_0, s3_0, s3_1, s4_0, s4_1, s4_2]
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
  clearWires [l, e, g, and1, and2]
  return (altb, aeqb, agtb)

cmp4N :: (Component c a) => [a] -> [a] -> c [a]
cmp4N a b =
  let (x : a') = chunksOf 4 (reverse a)
      (y : b') = chunksOf 4 (reverse b)
  in  do
        c@(c0, c1, c2) <- cmp4 x y
        (  l , e , g ) <- foldM (\m f -> f m) c $ zipWith cmp4MSB a' b'
        clearWires [c0, c1, c2]
        return [l, e, g]

eq4 :: (Component c a) => [a] -> [a] -> c a
eq4 a b
  | length a /= 4 || length b /= 4
  = error "eq4: input length not qeual to 4"
  | otherwise
  = let [a0, a1, a2, a3] = a
        [b0, b1, b2, b3] = b
    in  do
          o0   <- nxorGate a0 b0
          o1   <- nxorGate a1 b1
          o2   <- nxorGate a2 b2
          o3   <- nxorGate a3 b3
          and0 <- o0 `and` o1
          and1 <- o2 `and` o3
          out  <- and0 `and` and1
          clearWires [o0, o1, o2, o3, and0, and1]
          return out

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

neq :: (Component c a) => [a] -> [a] -> c a
neq a b = eq a b >>= notGate

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
    clearWires (c : c' : s : ad)
    return (head x : res)
   where
    goo []            c     = return [c]
    goo (x      : []) c     = return (x ++ [c])
    goo (x : xs : ys) carry = do
      (ad, c ) <- adderC (tail x) (init xs)
      (s , c') <- fullAdder c (last xs) carry
      res      <- goo ((ad ++ [s]) : ys) c'
      clearWires (c : c' : s : ad)
      return (head x : res)

minimum_ :: (Component c a) => [[a]] -> c [a]
minimum_ [] = return []
minimum_ (x : xs) = go x xs
  where go t [] = return t
        go t (y : ys) = do
          cond <- t `le` y
          x' <- ifThenElse cond t y
          go x' ys

calc :: (Component c a) => [a] -> [a] -> ([a], [a], [a]) -> c [a]
calc c z (c1, x, y) = do
  e <- c `neq` c1
  one <- int 1
  zero <- int 0
  y' <- adder y one
  z' <- adder z one
  e' <- ifThenElse e one zero
  x' <- adder x e'
  minimum_ [y', z', x']

transform :: (Component c a) => [[a]] -> [[a]] -> [a] -> c [[a]]
transform s ns@(n : ns1) c = do
  one <- int 1
  n' <- adder n one
  r <- scanlM (calc c) n' $ zip3 s ns ns1
  return r

lev :: (Component c a) => [[a]] -> [[a]] -> c [a]
lev s1 s2 = do
  len <- traverse int [0 .. length s1]
  res <- foldlM (transform s1) len s2
  return (last res)
