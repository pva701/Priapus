
-- | Language expressions parser and ADT

module Language.Expr
       ( -- * Expressions
         Value (..)
       , Ident (..)
       , Op (..)
       , Expr (..)
       , expr
       , value
       , ident
       ) where

import Prelude (show)
import Universum hiding (Const, show, try)

import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Expr

import Language.Lexer
import Language.Types

-- | Value types
data Value
    = Num !Word8
    | Boolean !Bool
    deriving (Eq, Ord, Generic)

instance Show Value where
    show (Num n)         = show n
    show (Boolean True)  = "true"
    show (Boolean False) = "false"

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
reserved = S.fromList
    [ "int", "bool", "void"
    , "true", "false"
    , "fun", "start", "return", "while", "break"
    , "if", "else"
    ]

ident :: Parser Ident
ident = Ident <$> identifier (flip S.member reserved)

boolean :: Parser Value
boolean = Boolean <$> (True <$ rword "true" <|> False <$ rword "false")

value :: Parser Value
value = Num <$> number
    <|> boolean

term :: Parser Expr
term =
        parens expr
    <|> Const <$> value
    <|> try (Call <$> ident <*> parens (expr `sepBy` str ","))
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

expr :: Parser Expr
expr = makeExprParser term operators
