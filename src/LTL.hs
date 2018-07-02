module LTL
    ( LTL (..)
    , parseLTL

    , negNormalForm
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
    | And LTL LTL
    | XOp LTL
    | UOp LTL LTL
    | ROp LTL LTL
    deriving (Show, Generic)

negNormalForm :: LTL -> LTL
negNormalForm (Not (Not a)) = negNormalForm a
negNormalForm v@(Var _) = v
negNormalForm c@(BConst _) = c
negNormalForm x@(Not (Var _)) = x
negNormalForm (Not (BConst v)) = BConst (not v)
negNormalForm (Not (Or a b)) = negNormalForm (Not a) `And` negNormalForm (Not b)
negNormalForm (Not (And a b)) = negNormalForm (Not a) `Or` negNormalForm (Not b)
negNormalForm (Not (XOp e)) = XOp (negNormalForm $ Not e)
negNormalForm (Not (UOp a b)) = negNormalForm (Not a) `ROp` negNormalForm (Not b)
negNormalForm (Not (ROp a b)) = negNormalForm (Not a) `UOp` negNormalForm (Not b)
negNormalForm (Or a b) = negNormalForm a `Or` negNormalForm b
negNormalForm (And a b) = negNormalForm a `And` negNormalForm b
negNormalForm (XOp a) = XOp $ negNormalForm a
negNormalForm (UOp a b) = negNormalForm a `UOp` negNormalForm b
negNormalForm (ROp a b) = negNormalForm a `ROp` negNormalForm b

-- Parser --

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
    , [InfixL (And <$ rword "&&"), InfixL (UOp <$ rword "U"), InfixL (ROp <$ rword "R")]
    , [InfixL (Or <$ rword "||")]
    , [InfixL (impl <$ rword "->") ]
    ]
  where
    fop a = BConst True `UOp` a
    gop a = Not $ fop $ Not a
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
