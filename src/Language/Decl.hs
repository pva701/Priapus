
-- | Functions declarations parser

module Language.Decl where

import Universum hiding (Type, many)

import Text.Megaparsec

import Language.Expr (Expr, Ident (..), Value (..))
import qualified Language.Expr as Expr
import Language.Lexer
import Language.Stmt (Stmt, Type (..))
import qualified Language.Stmt as Stmt
import Language.Types

-- | Function declaration
data Decl = Decl
    { dReturnType :: !(Maybe Type)
    , dName       :: !Ident
    , dParams     :: ![(Type, Ident)]
    , dBody       :: !Stmt
    } deriving (Show, Eq, Generic)

-- | Program body
data Program = Program
    { progFuncs :: ![Decl]
    , progMain  :: !Stmt
    } deriving (Show, Eq, Generic)

retType :: Parser (Maybe Type)
retType = Just <$> Stmt.typename <|> Nothing <$ rword "void"

param :: Parser (Type, Ident)
param = (,) <$> Stmt.typename <*> Expr.ident

declaration :: Parser Decl
declaration = Decl <$ rword "fun"
    <*> retType
    <*> Expr.ident
    <*> parens (param `sepBy` symbol ",")
    <*> parens' "{" "}" Stmt.stmt

mainBody :: Parser Stmt
mainBody = rword "start" *> parens' "{" "}" Stmt.stmt

program :: Parser Program
program = Program <$> many declaration <*> mainBody
