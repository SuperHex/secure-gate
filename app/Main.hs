{-# LANGUAGE TypeApplications #-}

module Main where

import Circuit.Gates ((<+>))
import Circuit.Wire
import Circuit (eval, getCircuit)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans (liftIO)
import Data.Word (Word8)
import Network.Pair

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : []) -> do
      case x of
        "server" -> server
        "client" -> client
    _ -> print "Please give two numbers"
