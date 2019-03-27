{-# LANGUAGE TypeApplications #-}

module Main where

import           Circuit
import           Circuit.Gates
import           Circuit.Wire
import           Control.Monad.Trans       (liftIO)
import qualified Data.ByteString           as BS
import           Data.Word                 (Word8)
import           Language.Compiler.Circuit
import           Language.Core
import           Network.Pair
import           System.Environment        (getArgs)
import           System.ZMQ4.Monadic

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : y : []) ->
      let y' = BS.pack . fromBits . finiteToBits $ (read y :: Int)
      in  case x of
            "server" -> runZMQ $ do
              sock <- initServer "tcp://127.0.0.1:1145"
              runServer prog y' sock
            "client" -> runZMQ $ do
              sock <- initClient "tcp://127.0.0.1:1145"
              runClient y' sock
        -- y' = BS.pack . fromBits . finiteToBits $ (read y :: Int)
    --   in  do
    --         str <- eval x' y' prog
    --         print . fromFiniteBits @Int . toBits . BS.unpack $ str
    _ -> print "Please give two numbers"
 where
  prog = do
    a <- in64 Alice
    b <- in64 Bob
    runWith b a $ lam2 $ \x y ->
      let func = lam2 (.+) in if_ (x .<= y) x (app2 func x y)


