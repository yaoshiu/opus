{-# LANGUAGE OverloadedStrings #-}

module Parser (expr, program, single) where

import Control.Applicative (many)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, manyTill, takeWhile1P, try, (<|>), MonadParsec (..))
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import SExpr (SExpr (..))

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

number :: Parser SExpr
number = SNumber <$> lexeme (L.signed (pure ()) L.decimal)

symbolP :: Parser SExpr
symbolP = lexeme . try $ do
  s <- symbolName
  if s == "."
    then fail "`.` cannot be a symbol"
    else pure (SSymbol s)

symbolName :: Parser Text
symbolName = takeWhile1P (Just "symbol") isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c =
  isAlphaNum c
    || not (c `elem` ("'\",[]{}()#`;," :: String)) && not (isSpace c)

preserved :: Text -> SExpr -> Parser SExpr
preserved sym val = try (symbol sym) >> pure val

bool :: Parser SExpr
bool = preserved "#t" (SBoolean True) <|> preserved "#f" (SBoolean False)

quoted :: Parser SExpr
quoted = do
  _ <- lexeme (char '\'')
  e <- expr
  pure $ SPair (SSymbol "quote") $ SPair e SNil

stringP :: Parser SExpr
stringP =
  SString . T.pack
    <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

atom :: Parser SExpr
atom = try number <|> stringP <|> bool <|> quoted <|> symbolP

expr :: Parser SExpr
expr = atom <|> list

list :: Parser SExpr
list = between (symbol "(") (symbol ")") pairRest

pairRest :: Parser SExpr
pairRest = (SNil <$ lookAhead (symbol ")"))
  <|> do
    h <- expr
    (do
        _ <- symbol "."
        t <- expr
        pure $ SPair h t
      ) <|> (SPair h <$> pairRest)

program :: Parser [SExpr]
program = sc *> many expr <* eof

single :: Parser SExpr
single = sc *> expr <* eof
