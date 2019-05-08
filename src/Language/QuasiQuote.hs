{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.QuasiQuote where

import           Circuit.Class
import           Circuit.Gates
import qualified Control.Monad.Combinators.Expr as C
import           Control.Monad.Identity
import           Data.Data
import           Data.Generics
import           Data.Void
import           Data.Word
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Quote      as Q
import qualified Language.Haskell.TH.Syntax     as THS
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error

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
 | Input (Int :~> String)
 | Meta (AntiQuote String)
 deriving (Show, Data)

data AntiQuote s
  = AntiInt s
  | AntiInput s
  | AntiExpr s
  deriving (Show, Data)

type Parser = ParsecT Void String Identity

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol str = lexeme (L.symbol sc str)

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

(#) :: (Show a) => Parser a -> String -> IO ()
(#) p s = parseTest p s

genOp k name f = k (f <$ symbol name)
binary = genOp C.InfixL
prefix = genOp C.Prefix

opTable :: [[C.Operator Parser Expr]]
opTable =
  [ [prefix "-" Neg]
  , [binary "*" (:*), binary "/" (:/), binary "%" (:%)]
  , [binary "+" (:+), binary "-" (:-)]
  ]

expr :: Parser Expr
expr = C.makeExprParser term opTable

term :: Parser Expr
term = choice [parens exprApp, integer, var, metaVar]

nonAppTerm :: Parser Expr
nonAppTerm = choice $ fmap lexeme [parens exprApp, integer, var, metaVar]

nonAppExpr :: Parser Expr
nonAppExpr = C.makeExprParser nonAppTerm opTable

antiInt :: Parser Expr
antiInt = do
  void $ symbol "$int:"
  (Name name) <- var
  pure (Meta $ AntiInt name)

antiInput :: Parser Expr
antiInput = do
  void $ symbol "$in:"
  (Name name) <- var
  pure (Meta $ AntiInput name)

metaVar :: Parser Expr
metaVar = choice [try antiInt, try antiInput]

exprApp :: Parser Expr
exprApp = go
 where
  go = do
    e  <- nonAppExpr
    e' <- expr'
    case e' of
      (_ : _) -> pure $ foldl App e e'
      []      -> pure e
  expr' = lexeme $ many
    (  try
    $  notFollowedBy
         (choice [symbol "+", symbol "-", symbol "*", symbol "/", symbol "%"])
    *> nonAppExpr
    )

parseProg :: (Monad m) => Parser a -> String -> m a
parseProg p s = case parse p "" s of
  (Left  e) -> fail $ show e
  (Right r) -> return r

quoteProgExp :: String -> Q Exp
quoteProgExp str = do
  e <- parseProg (space *> exprApp <* eof) str
  Q.dataToExpQ (const Nothing `extQ` quoteAntiExp) e

quoteProgPat :: String -> Q Pat
quoteProgPat str = do
  e <- parseProg (space *> exprApp <* eof) str
  Q.dataToPatQ (const Nothing `extQ` quoteAntiPat) e

quoteAntiExp :: Expr -> Maybe (Q Exp)
quoteAntiExp (Meta (AntiInt ident)) =
  Just $ appE (conE (mkName "Int64")) (varE (mkName ident))
quoteAntiExp (Meta (AntiInput ident)) =
  Just $ appE (conE (mkName "Input")) (varE (mkName ident))
quoteAntiExp _ = Nothing

quoteAntiPat :: Expr -> Maybe (Q Pat)
quoteAntiPat (Meta (AntiInt ident)) =
  Just $ conP (mkName "Int64") [varP (mkName ident)]
quoteAntiPat (Meta (AntiInput ident)) =
  Just $ conP (mkName "Input") [varP (mkName ident)]
quoteAntiPat _ = Nothing

prog :: Q.QuasiQuoter
prog = Q.QuasiQuoter
  { Q.quoteExp  = quoteProgExp
  , Q.quotePat  = quoteProgPat
  , Q.quoteType = undefined
  , Q.quoteDec  = undefined
  }
