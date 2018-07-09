module LTL
       ( LTL (..)
       , SatisfiedVars (..)
       , parseLTL

       , ltlToBuchiAutomaton
       , ltlToGBA
       , propositionals

       -- for testing
       , closure
       , checkClosure
       , allMaxConsistent
       ) where

import           Universum            hiding (many, try)

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
    deriving (Show, Generic, Eq, Ord)

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
propositionals (Var i ex) = [(i, ex)]
propositionals (Not e) = propositionals e
propositionals (XOp e) = propositionals e
propositionals v = propositionals (lOp v) ++ propositionals (rOp v)

data Subexprs = Subexprs
    { cl      :: Set LTL -- corresponding ordered set
    , ordered :: [LTL]   -- for each i: all subexpressions of ordered[i] occur earlier in list
    } deriving (Eq, Ord, Show)

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

checkClosure :: Closure -> Either Text ()
checkClosure (f, Subexprs cl ls) = do
    guardEi (true `S.member` cl) $ "true doesn't belong to closure"
    guardEi (f `S.member` cl) $ "f doesn't belong to closure"
    forM_ ls $ \f1 -> do
        checkSubterm f1 (neg f1)
        case f1 of
            Not    _ -> pass
            BConst _ -> pass
            Var  _ _ -> pass
            XOp f2   -> checkSubterm f1 f2
            op       -> checkSubterm f1 (lOp op) >> checkSubterm f1 (rOp op)
  where
    checkSubterm f1 fsub =
        guardEi (fsub `S.member` cl) $ show f1 <> " belongs to closure, but " <> show fsub <> " no"
    guardEi False e = Left e
    guardEi True _  = Right ()

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
    gen p xs@(UOp _ _ : _) = goOneOf p xs
    gen p xs@(ROp _ _ : _) = goOneOf p xs
    gen p@(s, _) (e@(Or a b) : xs)
        | S.member a s || S.member b s = gen (addExp p e) xs
        | otherwise                    = gen p xs
    gen p@(s, _) xs@(e@(And a b) : _)
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
ltlToGBA f' = do
    let (f, subexprs) = closure f'
    let nodes = allMaxConsistent subexprs
    let initNode = Subexprs mempty []

    let isUntil (UOp _ _) = True
        isUntil _         = False
    let untils = filter isUntil (ordered subexprs)
    let finals = flip map untils $ \u@(UOp _ f2) ->
            S.fromList $ filter (\(Subexprs s _) -> f2 `S.member` s || u `S.notMember` s) nodes

    let ifAddEdge _ m2 (XOp f1)       = f1 `S.member` cl m2
        ifAddEdge m1 m2 e@(UOp f1 f2) = f2 `S.member` cl m1 || f1 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge m1 m2 e@(ROp f1 f2) = (And f1 f2) `S.member` cl m1 || f2 `S.member` cl m1 && e `S.member` cl m2
        ifAddEdge _ _ _               = False

    let trans1 = createTransitions $ do
          m2 <- nodes
          let a = subexprsGetSatisfiedVars m2
          m1 <- nodes
          guard (any (ifAddEdge m1 m2) (toList $ cl m1))
          pure (m1, a, m2)
    let trans2 = M.unionsWith (M.unionWith (<>)) $
                     map (\v -> M.singleton initNode (M.singleton (subexprsGetSatisfiedVars v) (S.singleton v)))
                         (filter (S.member f . cl) nodes)
    GenBuchiAutomaton (trans1 <> trans2) (S.singleton initNode) (S.fromList finals)

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

-- rws :: [Text] -- list of reserved words
-- rws = ["true", "false", "not", "X", "U", "R", "F", "G"]

-- ident :: Parser Lg.Ident
-- ident = Lg.Ident <$> identifier (`elem` rws)

term :: Parser LTL
term =
      parens ltlExpr
  <|> (BConst True  <$ rword "true")
  <|> (BConst False <$ rword "false")
  <|> (uncurry Var <$> parseExpr)
  where
    parseExpr = do
        e <- Lg.expr
        case e of
            Lg.Var ident -> pure (ident, e)
            _            -> pure (Lg.Ident $ "tmp" <> show (hash e), e)
