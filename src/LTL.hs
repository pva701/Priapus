module LTL
    ( LTL (..)
    , parseLTL
    ) where

import           Universum                  hiding (many, try)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

data LTL
    = Var String
    | BConst Bool
    | Not LTL
    | Or LTL LTL
    | XOp LTL
    | UOp LTL LTL
    deriving (Show, Generic)

parseLTL :: String -> Either PError LTL
parseLTL = runParser ltlExprTrim ""

-- Copied from here https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
type Parser = Parsec Void String
type PError = ParseError Char Void

-- phi = p | not phi | phi or phi | X phi | phi U phi | phi and phi | phi -> phi | phi R phi | F phi | G phi

ltlExprTrim :: Parser LTL
ltlExprTrim = space *> ltlExpr <* eof

ltlExpr :: Parser LTL
ltlExpr = makeExprParser term operators

operators :: [[Operator Parser LTL]]
operators =
    [ [ Prefix (Not <$ rword "not"), Prefix (XOp <$ rword "X"),
        Prefix (fop <$ rword "F"), Prefix (gop <$ rword "G")]
    , [InfixL (andop <$ rword "and"), InfixL (UOp <$ rword "U"), InfixL (rop <$ rword "R")]
    , [InfixL (Or <$ rword "or")]
    , [InfixL (impl <$ rword "->") ]
    ]
  where
    fop a = BConst True `UOp` a
    gop a = Not $ fop $ Not a
    rop a b = Not $ Not a `UOp` Not b
    andop a b = Not $ Not a `Or` Not b
    impl a b = Not a `Or` b

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["true", "false", "not", "or", "X", "U", "and", "->", "R", "F", "G"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x =
        if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

term :: Parser LTL
term =
      parens ltlExpr
  <|> (BConst True  <$ rword "true")
  <|> (BConst False <$ rword "false")
  <|> (Var <$> identifier)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"
