{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative (many)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, manyTill, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Number Integer
  | Symbol Text
  | String Text
  | Boolean Bool
  | List [Expr]
  deriving (Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Parser Expr
number = Number <$> lexeme (L.signed (pure ()) L.decimal)

symbolP :: Parser Expr
symbolP = Symbol <$> lexeme symbolName

symbolName :: Parser Text
symbolName = takeWhile1P (Just "symbol") isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c =
  isAlphaNum c
    || not (c `elem` ("'\",[]{}()#`;," :: String)) && not (isSpace c)

preserved :: Text -> Expr -> Parser Expr
preserved sym val = try (symbol sym) >> pure val

bool :: Parser Expr
bool = preserved "#t" (Boolean True) <|> preserved "#f" (Boolean False)

quoted :: Parser Expr
quoted = do
  _ <- symbol "'"
  e <- expr
  pure $ List [Symbol "quote", e]

stringP :: Parser Expr
stringP =
  String . T.pack
    <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

atom :: Parser Expr
atom = try number <|> stringP <|> bool <|> quoted <|> symbolP

expr :: Parser Expr
expr = atom <|> list

list :: Parser Expr
list = List <$> between (symbol "(") (symbol ")") (many expr)

program :: Parser [Expr]
program = sc *> many expr <* eof
