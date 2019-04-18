{-# LANGUAGE RankNTypes #-}

module Circuit.Gates where

import           Circuit
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import           Data.List.Split
import           Utils                (bytesToBits)

-- Note: All input bits are ordered as
--         LSB ------> MSB

type Index = Int

mkConst :: BS.ByteString -> Builder [Int]
mkConst s = traverse mkConstBit (bytesToBits $ BS.unpack s)

notGate :: Int -> Builder Int
notGate x = do
  k <- mkConstBit True
  freeXOR k x

notGate' :: Int -> Int -> Builder Int
notGate' i o = do
  k <- mkConstBit True
  freeXORHelper k i (Just o)

orGate :: Int -> Int -> Builder Int
orGate a b = do
  x  <- freeXOR a b
  an <- halfAND a b
  freeXOR x an

norGate :: Int -> Int -> Builder Int
norGate a b = orGate a b >>= notGate

nxorGate :: Int -> Int -> Builder Int
nxorGate a b = freeXOR a b >>= notGate

halfAdder :: Index -> Index -> Builder (Index, Index)
halfAdder w0 w1 = do
  o0 <- freeXOR w0 w1
  o1 <- halfAND w0 w1
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

halfSubtractor :: Index -> Index -> Builder (Index, Index)
halfSubtractor x y = do
  o0 <- freeXOR x y
  o1 <- notGate x
  o2 <- halfAND y o1
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

mux :: Index -> Index -> Index -> Builder Index
mux s a b = do
  n <- notGate s
  t <- halfAND s a
  f <- halfAND n b
  orGate t f

ifThenElse :: Index -> [Index] -> [Index] -> Builder [Index]
ifThenElse s a b = sequenceA (zipWith (mux s) a b)

-- length (Builder [Index]) := 3
compareBit :: Index -> Index -> Builder (Index, Index, Index)
compareBit a b = do
  na  <- notGate a
  nb  <- notGate b
  l   <- halfAND na b
  g   <- halfAND nb a
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

cmp4 :: [Index] -> [Index] -> Builder (Index, Index, Index)
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
          s1       <- halfAND a3 nb3
          s2_0     <- halfAND a2 nb2
          s2       <- halfAND s2_0 s5
          s3_0     <- halfAND a1 nb1
          s3_1     <- halfAND s5 s6
          s3       <- halfAND s3_0 s3_1
          s4_0     <- halfAND a0 nb0
          s4_1     <- halfAND s5 s6
          s4_2     <- halfAND s4_0 s4_1
          s4       <- halfAND s4_2 s7
          -- output A > B
          a_gt_b_0 <- orGate s1 s2
          a_gt_b_1 <- orGate s3 s4
          a_gt_b   <- orGate a_gt_b_0 a_gt_b_1
          -- output A = B
          a_eq_b_0 <- halfAND s5 s6
          a_eq_b_1 <- halfAND s7 s8
          a_eq_b   <- halfAND a_eq_b_0 a_eq_b_1
          -- output A < B
          a_lt_b   <- norGate a_gt_b a_eq_b
          return (a_lt_b, a_eq_b, a_gt_b)

cmp4MSB
  :: [Index]
  -> [Index]
  -> (Index, Index, Index)
  -> Builder (Index, Index, Index)
cmp4MSB a b (lin, ein, gin) = do
  (l, e, g) <- cmp4 a b
  -- A < B
  and1      <- halfAND l ein
  altb      <- orGate and1 lin
  -- A = B
  aeqb      <- halfAND e ein
  -- A > B
  and2      <- halfAND g ein
  agtb      <- orGate and2 gin
  return (altb, aeqb, agtb)

cmp4N :: [Index] -> [Index] -> Builder [Index]
cmp4N a b =
  let (x : a') = chunksOf 4 (reverse a)
      (y : b') = chunksOf 4 (reverse b)
  in  do
        c         <- cmp4 x y
        (l, e, g) <- foldM (\m f -> f m) c $ zipWith cmp4MSB a' b'
        return [l, e, g]

eq4 :: [Index] -> [Index] -> Builder Index
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
          and0 <- halfAND o0 o1
          and1 <- halfAND o2 o3
          halfAND and0 and1

eq4N :: [Index] -> [Index] -> Builder Index
eq4N a b =
  let a' = chunksOf 4 a
      b' = chunksOf 4 b
  in  do
        r <- zipWithM eq4 a' b'
        case r of
          []       -> error "eq4N: empty input"
          (x : xs) -> foldM (halfAND) x xs

lt :: [Index] -> [Index] -> Builder Index
lt a b = fmap head (cmp4N a b)

le :: [Index] -> [Index] -> Builder Index
le a b = do
  c <- cmp4N a b
  orGate (head c) (c !! 1)

gt :: [Index] -> [Index] -> Builder Index
gt a b = fmap (!! 2) (cmp4N a b)

ge :: [Index] -> [Index] -> Builder Index
ge a b = do
  c <- cmp4N a b
  orGate (c !! 1) (c !! 2)

eq :: [Index] -> [Index] -> Builder Index
eq = eq4N

basicMul :: [Index] -> [Index] -> Builder [Index]
basicMul a b = do
  let partial y x = mapM (halfAND x) y
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

