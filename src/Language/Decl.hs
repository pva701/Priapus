
-- | Functions declarations parser

module Language.Decl
       ( Decl (..)
       , Program (..)
       , program
       , parseProgram
       ) where

import Universum hiding (Type, many, try)

import Text.Megaparsec

import Language.Expr (Ident (..), Value)
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

type VarDef = (Type, Ident, Maybe Value)

-- | Program body
data Program = Program
    { progFuncs :: ![Decl]
    , progVars  :: ![VarDef]
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

varDef :: Parser VarDef
varDef = (<* symbol ";") $ (,,)
    <$> Stmt.typename
    <*> Expr.ident
    <*> optional (str "=" *> Expr.value)

declOrVarDef :: Parser (Either Decl VarDef)
declOrVarDef = Left <$> try declaration <|>
               Right <$> try varDef

declsAndVarDefs :: Parser ([Decl], [VarDef])
declsAndVarDefs = unzipEithers <$> many declOrVarDef
  where unzipEithers = foldl' go ([], [])
        go (dcls, defs) (Left dcl)  = (dcl:dcls, defs)
        go (dcls, defs) (Right def) = (dcls, def:defs)

mainBody :: Parser Stmt
mainBody = rword "start" *> parens' "{" "}" Stmt.stmt

program :: Parser Program
program = uncurry Program <$> declsAndVarDefs <*> mainBody

parseProgram :: FilePath -> Text -> Either PError Program
parseProgram = parse program
