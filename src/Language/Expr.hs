
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

import           Prelude              (show)
import           Universum            hiding (Const, show, try)

import           Data.List            (intercalate)
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           Language.Lexer
import           Language.Types

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
    deriving (Eq, Ord, Generic, IsString)

instance Show Ident where
    show (Ident nm) = show nm

-- | Expressions
data Expr
    = Const Value
    | Var Ident
    | Binop Op Expr Expr
    | Call Ident [Expr]
    deriving (Eq, Ord, Generic)

showWithBr :: Expr -> String
showWithBr x@(Const _)  = show x
showWithBr (Var (Ident nm))  = toString nm
showWithBr x@(Call _ _) = show x
showWithBr x            = "(" ++ show x ++ ")"

instance Show Expr where
    show (Const v) = show v
    show (Var v) = show v
    show (Binop op e1 e2) = showWithBr e1 ++ " " ++ show op ++ " " ++ showWithBr e2
    show (Call f exprs) = show f ++ "(" ++ intercalate "," (map show exprs) ++ ")"

-- | Binary operations
data Op
    = Add | Sub | Mul | Div | Mod          -- arithmetics
    | Eq  | NEq | LEq | GEq | Less | More  -- comparisons
    | And | Or                             -- bool
    deriving (Eq, Ord, Generic)

instance Show Op where
    show Add  = "+"
    show Sub  = "-"
    show Mul  = "*"
    show Div  = "/"
    show Mod  = "%"
    show Eq   = "=="
    show NEq  = "!="
    show LEq  = "<="
    show GEq  = ">="
    show Less = "<"
    show More = ">"
    show And  = "&&"
    show Or   = "||"

instance Hashable Op
instance Hashable Expr
instance Hashable Ident
instance Hashable Value

-- | Reserved words (cannot be identifiers)
reserved :: Set Text
reserved = S.fromList
    [ "int", "bool", "void"
    , "true", "false"
    , "fun", "start", "return", "while", "break", "atomic"
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
