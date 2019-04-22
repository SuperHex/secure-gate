{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit.Parser where

import           Circuit.Class
import           Circuit.Gates
import           Circuit.Wire
import           Control.Monad.Reader
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust)
import           System.ZMQ4.Monadic
import Data.IORef
import qualified Data.Map as M

data Header = Header
  { numWires :: Int
  , numInA   :: Int
  , numInB   :: Int
  , numOut   :: Int
  }

parseText :: LBS.ByteString -> Int
parseText bs = case LBS.readInt bs of
  Nothing       -> error $ "parsing error on " ++ LBS.unpack bs
  (Just (n, _)) -> n

parseLine :: FilePath -> IO [[LBS.ByteString]]
parseLine file = do
  f <- LBS.readFile file
  pure $ LBS.splitWith (== ' ') <$> LBS.splitWith (== '\n') f

parseHeader :: [[LBS.ByteString]] -> Header
parseHeader lines = Header (nums !! 1) (numIO !! 0) (numIO !! 1) (numIO !! 4)
 where
  nums  = fmap parseText (head lines)
  numIO = fmap parseText (lines !! 1)

parse :: [[LBS.ByteString]] -> Builder z ()
parse lines = do
  mapM_ go lines
 where
  go :: [LBS.ByteString] -> Builder z Int
  go [] = return 0
  go [_, _, i, o, _] =
    let (Just (i', _)) = LBS.readInt i
        (Just (o', _)) = LBS.readInt o
    in  notGate' i' o'
  go [_, _, i0, i1, o, g] =
    let (Just (i0', _)) = LBS.readInt i0
        (Just (i1', _)) = LBS.readInt i1
        (Just (o' , _)) = LBS.readInt o
    in  case g of
          "XOR" -> freeXORHelper i0' i1' (Just o')
          "AND" -> halfANDHelper i0' i1' (Just o')
          x     -> error $ "unknown gate " ++ show x
  go x = error $ "unknown pattern" ++ show x

parseRun :: FilePath -> Builder z [Int]
parseRun file = do
  lines <- liftIO $ parseLine file
  let header = parseHeader lines
  void $ inN (numInA header) Alice
  void $ inN (numInB header) Bob
  parse (drop 3 lines)
    >> pure [numWires header - numOut header .. numWires header - 1]
