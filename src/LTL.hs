module LTL
    ( LTL (..)
    , parseLTL
    ) where

import Universum hiding (many, try)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import Language.Lexer
import Language.Types

data LTL
    = Var Text
    | BConst Bool
    | Not LTL
    | Or LTL LTL
    | XOp LTL
    | UOp LTL LTL
    deriving (Show, Generic)

parseLTL :: Text -> Either PError LTL
parseLTL = runParser ltlExprTrim ""

-- Copied from here https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

-- phi = p | not phi | phi || phi | X phi | phi U phi | phi && phi | phi -> phi | phi R phi | F phi | G phi

ltlExprTrim :: Parser LTL
ltlExprTrim = space *> ltlExpr <* eof

ltlExpr :: Parser LTL
ltlExpr = makeExprParser term operators

operators :: [[Operator Parser LTL]]
operators =
    [ [ Prefix (Not <$ rword "not"), Prefix (XOp <$ rword "X"),
        Prefix (fop <$ rword "F"), Prefix (gop <$ rword "G")]
    , [InfixL (andop <$ rword "&&"), InfixL (UOp <$ rword "U"), InfixL (rop <$ rword "R")]
    , [InfixL (Or <$ rword "||")]
    , [InfixL (impl <$ rword "->") ]
    ]
  where
    fop a = BConst True `UOp` a
    gop a = Not $ fop $ Not a
    rop a b = Not $ Not a `UOp` Not b
    andop a b = Not $ Not a `Or` Not b
    impl a b = Not a `Or` b

rws :: [Text] -- list of reserved words
rws = ["true", "false", "not", "X", "U", "R", "F", "G"]

ident :: Parser Text
ident = identifier (`elem` rws)

term :: Parser LTL
term =
      parens ltlExpr
  <|> (BConst True  <$ rword "true")
  <|> (BConst False <$ rword "false")
  <|> (Var <$> ident)
