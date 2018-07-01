
-- | Language statemets parser and ADT

module Language.Stmt
       ( Stmt (..)
       , stmt
       ) where

import Universum hiding (Type, break, many, some, try)

import Text.Megaparsec

import qualified Data.DList as DL
import Language.Expr (Expr, Ident (..), Value (..))
import qualified Language.Expr as Expr
import Language.Lexer
import Language.Types

-- | Type names
data Type = Bool' | Int'
    deriving (Eq, Ord, Show, Generic)

-- | Language statements
data Stmt
    = Seq Stmt Stmt
    | Skip
    | Declare Type Ident
    | Assign Ident Expr
    | If Expr Stmt Stmt
    | While Expr Stmt
    | Break
    | Return (Maybe Expr)
    | Call Ident [Expr]
    deriving (Eq, Ord, Show, Generic)

assignment :: Parser Stmt
assignment = Assign <$> (Expr.ident <* str "=") <*> Expr.expr

typename :: Parser Type
typename = Bool' <$ rword "bool" <|> Int' <$ rword "int"

declaration :: Parser Stmt
declaration = do
    t <- typename
    let onlyDec = Declare t <$> Expr.ident
        decAssign = do
            x <- Expr.ident <* str "="
            v <- Expr.expr
            pure $ Seq (Declare t x) (Assign x v)
    listToSeq <$> (try decAssign <|> onlyDec) `sepBy` str ","

break :: Parser Stmt
break = Break <$ rword "break"

ret :: Parser Stmt
ret = Return <$ rword "return" <*> optional Expr.expr

call :: Parser Stmt
call = Call <$> Expr.ident <*> parens (Expr.expr `sepBy` str ",")

block :: Parser Stmt
block = try (between (symbol "{") (symbol "}") (listToSeq <$> many singleStmt))
    <|> singleStmt

ifElse :: Parser Stmt
ifElse = (If <$ rword "if") <*> parens Expr.expr <*> block
    <*> (maybeToStmt <$> optional (rword "else" *> block))

while :: Parser Stmt
while = (While <$ rword "while") <*> parens Expr.expr <*> block

singleStmt :: Parser Stmt
singleStmt =
        try declaration <* symbol ";"
    <|> try assignment <* symbol ";"
    <|> try break <* symbol ";"
    <|> try ret <* symbol ";"
    <|> try call <* symbol ";"
    <|> try ifElse
    <|> try while

stmt :: Parser Stmt
stmt = listToSeq <$> some singleStmt

------------------------------------------------------------
-- Statement sequence normalization
------------------------------------------------------------

maybeToStmt :: Maybe Stmt -> Stmt
maybeToStmt (Just s) = s
maybeToStmt Nothing  = Skip

seqToList :: Stmt -> [Stmt]
seqToList (Seq a b) = a : seqToList b
seqToList s         = [s]

listToSeq :: [Stmt] -> Stmt
listToSeq [] = Skip
listToSeq ls = foldl1 Seq ls

-- straightenSeq :: Stmt -> Stmt
-- straightenSeq (Seq s@(Seq _ _) c) =
--     case straightenSeq s of
--         Seq a as -> Seq a (Seq c as)
-- straightenSeq (Seq s Skip) = straightenSeq s
-- straightenSeq (Seq Skip s) = straightenSeq s
-- straightenSeq s            = s


