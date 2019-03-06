module Circuit.Gates where

import Circuit
import Control.Monad

halfAdder :: Int -> Int -> Builder (Int, Int)
halfAdder w0 w1 = do
  o0 <- mkGate XOR w0 w1
  o1 <- mkGate AND w0 w1
  return (o0, o1)

fullAdder :: Int -> Int -> Int -> Builder (Int, Int)
fullAdder a b c = do
  (s0, c0) <- halfAdder a b
  (s1, c1) <- halfAdder s0 c
  out      <- mkGate OR c0 c1
  return (s1, out)

adderN :: [Int] -> [Int] -> Int -> Builder [Int]
adderN []       []       _     = return []
adderN (x : xs) (y : ys) carry = do
  (s0, c0) <- fullAdder x y carry
  out      <- adderN xs ys c0
  return (s0 : out)

adder :: [Int] -> [Int] -> Builder [Int]
adder [] [] = return []
adder x y | length x /= length y = error "input wire lists' length differ!"
adder (x : xs) (y : ys) = do
  (s, c) <- halfAdder x y
  sums   <- adderN xs ys c
  return (s : sums)

(<+>) = adder

halfSubtractor :: Int -> Int -> Builder (Int, Int)
halfSubtractor x y = do
  o0 <- mkGate XOR x y
  o1 <- notGate x
  o2 <- mkGate AND y o1
  return (o0, o2)

fullSubtractor :: Int -> Int -> Int -> Builder (Int, Int)
fullSubtractor x y bin = do
  (d   , b ) <- halfSubtractor x y
  (diff, b') <- halfSubtractor d bin
  bout       <- mkGate OR b b'
  return (diff, bout)

subtractorN :: [Int] -> [Int] -> Int -> Builder [Int]
subtractorN []       []       _      = return []
subtractorN (x : xs) (y : ys) borrow = do
  (diff, bout) <- fullSubtractor x y borrow
  out          <- subtractorN xs ys bout
  return (diff : out)

subtractor :: [Int] -> [Int] -> Builder [Int]
subtractor [] [] = return []
subtractor x y | length x /= length y = error "input wire lists' length differ!"
subtractor (x : xs) (y : ys) = do
  (d, b) <- halfSubtractor x y
  diffs  <- subtractorN xs ys b
  return (d : diffs)

(<->) = subtractor
