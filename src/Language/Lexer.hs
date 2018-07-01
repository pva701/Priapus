
-- | Basic types and utils

module Language.Lexer
       ( lexeme
       , symbol
       , parens
       , str
       , rword
       , identifier
       , number
       ) where

import Universum hiding (try)

import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Types

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

str :: Text -> Parser ()
str = void . lexeme . try . string

rword :: Text -> Parser ()
rword s = lexeme . try $ string s *> notFollowedBy alphaNumChar

identifier :: (Text -> Bool) -> Parser Text
identifier isReserved = lexeme . try $ p >>= check
  where
    p       = T.cons <$> letterChar <*> takeWhileP Nothing isAlphaNum
    check x =
        if isReserved x
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

number :: Parser Int
number = L.signed sc $ lexeme L.decimal
