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

adder :: [Int] -> [Int] -> Int -> Builder [Int]
adder []       []       _     = return []
adder []       (_ : _)  _     = error "input wire lists' length differ!"
adder (_ : _ ) []       _     = error "input wire lists' length differ!"
adder (x : xs) (y : ys) carry = do
  (s0, c0) <- fullAdder x y carry
  out      <- adder xs ys c0
  return (s0 : out)

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

subtractor :: [Int] -> [Int] -> Int -> Builder [Int]
subtractor []       []       _      = return []
subtractor []       (_ : _)  _      = error "input wire lists' length differ!"
subtractor (_ : _ ) []       _      = error "input wire lists' length differ!"
subtractor (x : xs) (y : ys) borrow = do
  (diff, bout) <- fullSubtractor x y borrow
  out          <- subtractor xs ys bout
  return (diff : out)
