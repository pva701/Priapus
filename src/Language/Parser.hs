
-- | Language parser and ADT

module Language.Parser
       ( -- * Expressions
         Value (..)
       , Ident (..)
       , Expr (..)
       , langExpr
       ) where

import Universum hiding (Const, try)

import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import Language.Lexer
import Language.Types

--------------------------------------------------
-- Expressions
--------------------------------------------------

-- | Value types
data Value
    = Num !Int
    | Boolean !Bool
    deriving (Eq, Ord, Show, Generic)

-- | Identifiers
newtype Ident = Ident Text
    deriving (Eq, Ord, Show, Generic, IsString)

-- | Expressions
data Expr
    = Const Value
    | Var Ident
    | Binop Op Expr Expr
    | Call Ident [Expr]
    deriving (Eq, Ord, Show, Generic)

-- | Binary operations
data Op
    = Add | Sub | Mul | Div | Mod          -- arithmetics
    | Eq  | NEq | LEq | GEq | Less | More  -- comparisons
    | And | Or                             -- bool
    deriving (Eq, Ord, Show, Generic)

-- | Reserved words (cannot be identifiers)
reserved :: Set Text
reserved = S.fromList ["int", "bool", "true", "false", "fun", "start", "return", "while", "break"]

ident :: Parser Ident
ident = Ident <$> identifier (flip S.member reserved)

boolean :: Parser Value
boolean = Boolean <$> (True <$ rword "true" <|> False <$ rword "false")

value :: Parser Value
value = Num <$> number
    <|> boolean

langTerm :: Parser Expr
langTerm =
        parens langExpr
    <|> Const <$> value
    <|> try (Call <$> ident <*> parens (langExpr `sepBy` str ","))
    <|> Var <$> ident

operators :: [[Operator Parser Expr]]
operators =
    [ allOp InfixL [(Mul, "*"), (Div, "/"), (Mod, "%")]
    , allOp InfixL [(Add, "+"), (Sub, "-")]
    , allOp InfixN [(Eq, "=="), (NEq, "!="), (LEq, "<="), (GEq, ">="), (Less, "<"), (More, ">")]
    , allOp InfixL [(And, "&&")]
    , allOp InfixL [(Or, "||")]
    ]
  where
    allOp inf = map $ \(op, sym) -> inf $ Binop op <$ str sym

langExpr :: Parser Expr
langExpr = makeExprParser langTerm operators
