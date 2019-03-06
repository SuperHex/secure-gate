{-# LANGUAGE TypeApplications #-}

module Main where

import Circuit.Gates ((<+>))
import Circuit.Wire
import Circuit (eval, getCircuit)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans (liftIO)
import Data.Word (Word8)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : y : xs) -> do
      circuit <- getCircuit adderC
      print circuit
      result <- eval @Int (read x :: Int) (read y :: Int) adderC
      print result
    _ -> print "Please give two numbers"
 where
  adderC = do
    a <- in64
    b <- in64
    a <+> b
