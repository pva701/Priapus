module LTL
       ( LTL (..)
       , SatisfiedVars (..)
       , parseLTL

       , ltlToBuchiAutomaton
       , ltlToGBA
       , propositionals

       -- for testing
       , closure
       , allMaxConsistent
       ) where

import           Universum            hiding (many, try, show)
import           Prelude (show)

import           Data.Hashable        (Hashable (..))
import           Data.List            (nub)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr

import           Buchi                (BuchiAutomaton (..),
                                       GenBuchiAutomaton (..), Layer,
                                       createTransitions, gbaToBuchiAutomaton)
import qualified Language.Expr        as Lg
import           Language.Lexer
import           Language.Types

data LTL
    = Var Lg.Ident Lg.Expr
    | BConst Bool
    | Not LTL
    | XOp LTL
    | Or  {lOp :: LTL, rOp :: LTL}
    | And {lOp :: LTL, rOp :: LTL}
    | UOp {lOp :: LTL, rOp :: LTL}
    | ROp {lOp :: LTL, rOp :: LTL}
    deriving (Generic, Eq, Ord)

showWithBr :: LTL -> String
showWithBr x@(Var _ _) = show x
showWithBr x@(BConst _) = show x
showWithBr x@(XOp _) = show x
showWithBr x = show x

instance Show LTL where
    show (Var _ ex) = show ex
    show (BConst True) = "true"
    show (BConst False) = "false"
    show (Not x) = "not " ++ showWithBr x
    show (XOp x) = "X " ++ showWithBr x
    show (Or e1 e2) = showWithBr e1 ++ " || " ++ showWithBr e2
    show (And e1 e2) = showWithBr e1 ++ " && " ++ showWithBr e2
    show (UOp e1 e2) = showWithBr e1 ++ " U " ++ showWithBr e2
    show (ROp e1 e2) = showWithBr e1 ++ " R " ++ showWithBr e2

true :: LTL
true = BConst True

-- LTL to BA (easy variant)

neg :: LTL -> LTL
neg (BConst x) = BConst (not x)
neg (Not a)    = a
neg a          = Not a

negNormalForm :: LTL -> LTL
negNormalForm (Not (Not a))    = negNormalForm a
negNormalForm v@(Var _ _)      = v
negNormalForm c@(BConst _)     = c
negNormalForm x@(Not (Var _ _))  = x
negNormalForm (Not (BConst v)) = BConst (not v)
negNormalForm (Not (Or a b))   = negNormalForm (Not a) `And` negNormalForm (Not b)
negNormalForm (Not (And a b))  = negNormalForm (Not a) `Or` negNormalForm (Not b)
negNormalForm (Not (XOp e))    = XOp (negNormalForm $ Not e)
negNormalForm (Not (UOp a b))  = negNormalForm (Not a) `ROp` negNormalForm (Not b)
negNormalForm (Not (ROp a b))  = negNormalForm (Not a) `UOp` negNormalForm (Not b)
negNormalForm (XOp a)          = XOp $ negNormalForm a
negNormalForm (Or a b)         = negNormalForm a `Or` negNormalForm b
negNormalForm (And a b)        = negNormalForm a `And` negNormalForm b
negNormalForm (UOp a b)        = negNormalForm a `UOp` negNormalForm b
negNormalForm (ROp a b)        = negNormalForm a `ROp` negNormalForm b


propositionals :: LTL -> [(Lg.Ident, Lg.Expr)]
propositionals = S.toList . S.fromList . propositionalsDo
  where
    propositionalsDo :: LTL -> [(Lg.Ident, Lg.Expr)]
    propositionalsDo (Var i ex) = [(i, ex)]
    propositionalsDo (Not e) = propositionalsDo e
    propositionalsDo (XOp e) = propositionalsDo e
    propositionalsDo (BConst _) = []
    propositionalsDo v = propositionalsDo (lOp v) ++ propositionalsDo (rOp v)

data Subexprs = Subexprs
    { cl      :: Set LTL -- corresponding ordered set
    , ordered :: [LTL]   -- for each i: all subexpressions of ordered[i] occur earlier in list
    } deriving (Eq, Ord)

instance Show Subexprs where
    show (Subexprs cl _) = "{" ++ intercalate "," (map show $ toList cl) ++ "}"

fromOrdered :: [LTL] -> Subexprs
fromOrdered ls =
    let nb = nub ls in Subexprs (S.fromList nb) nb

type Closure = (LTL, Subexprs)

closure :: LTL -> Closure
closure f =
    let f' = negNormalForm f in
    let ordered = reverse $ buildCL f' in
    let ordered' = true : ordered in
    (f', fromOrdered ordered')
  where
    buildCL :: LTL -> [LTL]
    buildCL p@(Var _ _)  = [neg p, p]
    buildCL c@(BConst _) = [neg c, c]
    buildCL e@(Not a)    = e : buildCL a
    buildCL e@(XOp a)    = neg e : e : buildCL a
    buildCL e            = neg e : e : buildCL (lOp e) ++ buildCL (rOp e)

type MaxConsistents = [Subexprs]

allMaxConsistent :: Subexprs -> MaxConsistents
allMaxConsistent (Subexprs _ ls) = execState (gen (S.singleton true, one true) ls) mempty
  where
    addExp (s, l) e = (S.insert e s, e : l)

    gen :: (Set LTL, [LTL]) -> [LTL] -> State MaxConsistents ()
    gen (s, l) [] = modify (Subexprs s (reverse l) :)
    gen p (BConst _ : xs)  = gen p xs
    gen p xs@(Var _ _ : _)   = goOneOf p xs
    gen p@(s, _) (x@(Not e) : xs)
        | S.member e s     = gen p xs
        | otherwise        = gen (addExp p x) xs
    gen p xs@(XOp _ : _)   = goOneOf p xs
    gen p@(s, _) xx@(x@(UOp a b) : xs)
        | S.member b s     = gen (addExp p x) xs
        | S.notMember a s  = gen p xs
        | otherwise        = goOneOf p xx
    gen p@(s, _) xx@(x@(ROp a b) : xs)
        | S.member (a `And` b) s = gen (addExp p x) xs
        | S.notMember b s        = gen p xs
        | otherwise              = goOneOf p xx
    gen p@(s, _) (e@(Or a b) : xs)
        | S.member a s || S.member b s = gen (addExp p e) xs
        | otherwise                    = gen p xs
    gen p@(s, _) (e@(And a b) : xs)
        | S.member a s && S.member b s = gen (addExp p e) xs
        | otherwise                    = gen p xs

    goOneOf _ [] = error "goOneOf: invalid call"
    goOneOf p (x : xs) = do
        gen (addExp p x) xs
        gen p xs

newtype SatisfiedVars = SatisfiedVars (Set Lg.Ident)
    deriving (Eq, Ord, Show)

subexprsGetSatisfiedVars :: Subexprs -> SatisfiedVars
subexprsGetSatisfiedVars = SatisfiedVars . S.fromList . mapMaybe toSatisfiedVar . ordered
  where
    toSatisfiedVar (Var x _) = Just x
    toSatisfiedVar _         = Nothing

ltlToGBA :: LTL -> GenBuchiAutomaton SatisfiedVars Subexprs
ltlToGBA f' = runIdentity $ do
    let (f, subexprs) = closure f'
    let nodes = allMaxConsistent subexprs
    let initNode = Subexprs mempty []

    let isUorR (UOp _ _) = True
        isUorR (ROp _ _) = True
        isUorR _         = False
    let untils = filter isUorR (ordered subexprs)
    -- TODO ADD ROp
    let finals = flip map untils $ \case
            u@(UOp _ f2)  -> S.fromList $ filter (\(Subexprs s _) -> f2 `S.member` s || u `S.notMember` s) nodes
            r@(ROp f1 f2) -> S.fromList $ filter (\(Subexprs s _) -> (f1 `S.member` s && f2 `S.member` s) || r `S.notMember` s) nodes
            _             -> error "unexpected operator"

    let ifAddEdge _ m2 (XOp f1)             = f1 `S.member` cl m2
        ifAddEdge _ m2 (Not (XOp f1))       = not (f1 `S.member` cl m2)
        ifAddEdge m1 m2 e@(UOp f1 f2)       = f2 `S.member` cl m1 || f1 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge m1 m2 e@(Not (UOp f1 f2)) = not $ f2 `S.member` cl m1 || f1 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge m1 m2 e@(ROp f1 f2)       = f1 `S.member` cl m1 && f2 `S.member` cl m1 || f2 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge m1 m2 e@(Not (ROp f1 f2)) = not $ f1 `S.member` cl m1 && f2 `S.member` cl m1 || f2 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge _ _ _                     = True

    let trans1 = createTransitions $ do
          m2 <- nodes
          let a = subexprsGetSatisfiedVars m2
          m1 <- nodes
          guard (all (ifAddEdge m1 m2) (toList $ cl m1))
          pure (m1, a, m2)
    let trans2 = M.unionsWith (M.unionWith (<>)) $
                     map (\v -> M.singleton initNode (M.singleton (subexprsGetSatisfiedVars v) (S.singleton v)))
                         (filter (S.member f . cl) nodes)
    pure $ GenBuchiAutomaton (trans1 <> trans2) (S.singleton initNode) (S.fromList finals)

ltlToBuchiAutomaton :: LTL -> BuchiAutomaton SatisfiedVars (Subexprs, Layer)
ltlToBuchiAutomaton = gbaToBuchiAutomaton . ltlToGBA

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

ident :: Parser (Lg.Ident, Lg.Expr)
ident = do
    i <- Lg.Ident <$> identifier (`elem` rws)
    pure (i, Lg.Var i)

term :: Parser LTL
term =
      parens ltlExpr
  <|> (BConst True  <$ rword "true")
  <|> (BConst False <$ rword "false")
  <|> (uncurry Var <$> ident)
  <|> (uncurry Var <$> parsePred)
  where
    parsePred = do
        void $ symbol "["
        e <- Lg.expr
        res <- case e of
            Lg.Var i -> pure (i, e)
            _        -> pure (Lg.Ident $ toText $ "tmp" <> show (hash e), e)
        void $ symbol "]"
        pure res
