
-- | Language statemets parser and ADT

module Language.Stmt
       ( Stmt (..)
       , StmtId (..)
       , Scope (..)
       , Type (..)
       , stmt
       , typename
       ) where

import Prelude (show)
import Universum hiding (Type, break, many, show, some, try)

import Text.Megaparsec
import Text.Megaparsec.Pos (SourcePos (..), unPos)

import Language.Expr (Expr, Ident (..))
import qualified Language.Expr as Expr
import Language.Lexer
import Language.Types

-- | Type names
data Type = Bool' | Int'
    deriving (Eq, Ord, Generic)

instance Show Type where
    show Bool' = "bool"
    show Int'  = "int"

-- | Statement id (basically simplified source position)
data StmtId = StmtId !Int !Int
    deriving (Eq, Ord, Show, Generic)

-- | Signifies the boundaries of a new scope
data Scope = Scope
    { scopeBegin :: !StmtId
    , scopeBody  :: !Stmt
    , scopeEnd   :: !StmtId
    } deriving (Eq, Ord, Show, Generic)

-- | Language statements
data Stmt
    = Seq Stmt Stmt
    | Skip
    | Declare Type StmtId Ident
    | Assign StmtId Ident Expr
    | If Expr Scope Scope
    | While Expr Scope
    | Break StmtId
    | Return StmtId (Maybe Expr)
    | Call Ident [Expr]
    | Atomic StmtId Stmt
    deriving (Eq, Ord, Show, Generic)

stmtId :: Parser StmtId
stmtId = do
    SourcePos {..} <- getPosition
    return $ StmtId (unPos sourceLine) (unPos sourceColumn)

assignment :: Parser Stmt
assignment = Assign <$> stmtId <*> (Expr.ident <* str "=") <*> Expr.expr

typename :: Parser Type
typename = Bool' <$ rword "bool" <|> Int' <$ rword "int"

declaration :: Parser Stmt
declaration = do
    t <- typename
    let onlyDec = Declare t <$> stmtId <*> Expr.ident
        decAssign = do
            dId <- stmtId
            x <- Expr.ident <* str "="
            aId <- stmtId
            v <- Expr.expr
            pure $ Seq (Declare t dId x) (Assign aId x v)
    flattenSeqs <$> (try decAssign <|> onlyDec) `sepBy` str ","

break :: Parser Stmt
break = Break <$> stmtId <* rword "break"

ret :: Parser Stmt
ret = Return <$> stmtId <* rword "return" <*> optional Expr.expr

call :: Parser Stmt
call = Call <$> Expr.ident <*> parens (Expr.expr `sepBy` str ",")

block :: Parser Stmt
block = try (parens' "{" "}" (flattenSeqs <$> many singleStmt))
    <|> singleStmt

scope' :: Parser Stmt -> Parser Scope
scope' p = Scope <$> stmtId <*> p <*> stmtId

scope :: Parser Scope
scope = scope' block

ifElse :: Parser Stmt
ifElse = (If <$ rword "if") <*> parens Expr.expr <*> scope
    <*> (scope' $ maybeToStmt <$> optional (rword "else" *> block))

while :: Parser Stmt
while = (While <$ rword "while") <*> parens Expr.expr <*> scope

atomic :: Parser Stmt
atomic = (Atomic <$> stmtId <* rword "atomic") <*> block

singleStmt :: Parser Stmt
singleStmt =
        try declaration <* symbol ";"
    <|> try assignment <* symbol ";"
    <|> try break <* symbol ";"
    <|> try ret <* symbol ";"
    <|> try call <* symbol ";"
    <|> try ifElse
    <|> try while
    <|> try atomic

stmt :: Parser Stmt
stmt = flattenSeqs <$> some singleStmt

------------------------------------------------------------
-- Statement sequence normalization
------------------------------------------------------------

maybeToStmt :: Maybe Stmt -> Stmt
maybeToStmt (Just s) = s
maybeToStmt Nothing  = Skip

listToSeq :: [Stmt] -> Stmt
listToSeq [] = Skip
listToSeq ls = foldr1 Seq ls

seqToList :: Stmt -> [Stmt]
seqToList s = fromMaybe [s] $ seqToList' s
  where
    seqToList' :: Stmt -> Maybe [Stmt]
    seqToList' (Seq a b) = Just $ seqToList a ++ seqToList b
    seqToList' Skip      = Just []
    seqToList' _         = Nothing

-- flattenSeq :: Stmt -> Stmt
-- flattenSeq = listToSeq . seqToList

flattenSeqs :: [Stmt] -> Stmt
flattenSeqs = listToSeq . concatMap seqToList
