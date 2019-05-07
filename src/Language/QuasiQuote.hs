{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.QuasiQuote where

import qualified Control.Monad.Combinators.Expr as C
import           Control.Monad.Identity
import           Data.Data
import           Data.Void
import           Data.Word
import           Language.Haskell.Meta
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Quote      as Q
import qualified Language.Haskell.TH.Syntax     as THS
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

class (Num a) => NumType a
instance NumType Int
instance NumType Word8

s :: (THS.Lift a) => ExpQ -> ExpQ -> [a] -> ExpQ
s f acc []       = [| [] |]
s f acc (x : xs) = [| let t = $f $acc x in t : $(s f [|t|] xs)|]

data Expr
 = Name String
 | Int64 Int
 | Expr :+ Expr
 | Expr :- Expr
 | Expr :* Expr
 | Expr :/ Expr
 | Expr :% Expr
 | Neg Expr
 | App Expr Expr
 | AntiInt String
  deriving (Show, Data)


type Parser = ParsecT Void String Identity

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

var :: Parser Expr
var = Name <$> do
  c  <- letterChar
  cs <- many alphaNumChar
  pure (c : cs)

integer :: Parser Expr
integer = Int64 <$> lexeme L.decimal

charLit :: Parser Char
charLit = lexeme L.charLiteral

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

(#) p s = parseTest p s

genOp k name f = k (f <$ symbol name)
binary = genOp C.InfixL
prefix = genOp C.Prefix

table :: [[C.Operator Parser Expr]]
table =
  [ [prefix "-" Neg, binary " " App]
  , [binary "*" (:*), binary "/" (:/), binary "%" (:%)]
  , [binary "+" (:+), binary "-" (:-)]
  ]

expr :: Parser Expr
expr = C.makeExprParser term table

term :: Parser Expr
term = choice [parens expr, integer, var, antiInt]

antiInt :: Parser Expr
antiInt = lexeme $ do
  void $ symbol "$int:"
  (Name name) <- var
  pure (AntiInt name)

parseProg :: (Monad m) => Parser a -> String -> m a
parseProg p s = case parse p "" s of
  (Left  e) -> fail $ show e
  (Right r) -> return r

quoteProgExp :: String -> Q Exp
quoteProgExp str = do
  l <- location
  e <- parseProg (space *> expr <* eof) str
  Q.dataToExpQ (const Nothing) e

prog = Q.QuasiQuoter {Q.quoteExp = quoteProgExp}

