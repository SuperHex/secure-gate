{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Circuit.Class
import           Circuit.Eval
import           Circuit.Gates
import           Circuit.Parser
import           Circuit.Wire
import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.List.Split            (chunksOf)
import           Data.Word                  (Word8)
import           Language.Compiler.Circuit
import           Language.Core
import           Language.Extension
import           Language.QuasiQuote
import           Network.Pair
import           System.Environment         (getArgs)
import           System.ZMQ4.Monadic
import           Utils

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : xs) -> case x of
      "server" ->
        let y' = case xs of
              (_ : y : []) ->
                BS.pack . bitsToBytes . fromFinite $ (read y :: Int)
              (y : []) -> BSC.pack y
        in  runZMQ $ do
              sock <- initServer "tcp://127.0.0.1:1145"
              runServer y' prog1 sock
      "client" ->
        let y' = case xs of
              (_ : y : []) ->
                BS.pack . bitsToBytes . fromFinite $ (read y :: Int)
              (y : []) -> BSC.pack y
        in  runZMQ $ do
              sock <- initClient "tcp://127.0.0.1:1145"
              hex  <- runClient y' sock
              liftIO . putStrLn . LBSC.unpack $ hex
        -- y' = BS.pack . fromBits . fromFinite $ (read y :: Int)
    --   in  do
    --         str <- eval x' y' prog
    --         print . fromFiniteBits @Int . toBits . BS.unpack $ str
    _ -> print "Please give two numbers"
 where
  prog2 = do
    a <- in64 Alice
    b <- in64 Bob
    runWith a b
      $ leven (fmap word8 (BS.unpack "abc")) (fmap word8 (BS.unpack "abc"))

  prog1 = do
    alice <- view @String $ in64 Alice
    bob   <- view @String $ in64 Bob
    let three = 3 :: Int
    secureRun [prog| $in:alice - $in:bob + (3 - $int:three) |]
