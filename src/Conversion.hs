
-- | Conversion between program automaton and Buchi one

module Conversion where

import Universum

import qualified Data.Map as M
import qualified Data.Set as S

import Buchi (BuchiAutomaton (..), Transitions)
import Language.Expr (Expr, Ident (..), Value (..))
import Language.Interpret (Env, EvalAutomaton (..), IState (..), InterpretError (..), StateId,
                           evalExpr)
import Language.Stmt (StmtId (..), Type (..))
import LTL (SatisfiedVars (..))

-- | Function calls in considered formulas are prohibited,
-- so we don't bother with reader state.
evalExprInEnv :: Expr -> Env -> Either InterpretError Value
evalExprInEnv e env = usingReaderT mempty $
    evalStateT (evalExpr e) initState
  where
    initStateId = (StmtId (-1) (-1), env)
    initAutomaton = EvalAutomaton mempty mempty
    initState = IState initStateId initAutomaton 0

type VarMapping = [(Ident, Expr)]

-- | Given a 'VarMapping' and a program env, get a set of
-- propositional variables which are satisfied in this env
satisfiedInEnv :: Env -> VarMapping -> Either InterpretError SatisfiedVars
satisfiedInEnv env = fmap SatisfiedVars . foldM checkVar mempty
  where checkVar xs (x, e) = case evalExprInEnv e env of
            Left (UndefinedVar _) -> Right xs
            Left err              -> Left err
            Right v -> case v of
                Boolean True  -> Right $ S.insert x xs
                Boolean False -> Right xs
                Num _         -> Left $ TypeMismatch Int' Bool'

addTransition
    :: forall alph state. (Ord alph, Ord state)
    => state -> alph -> state -> Transitions alph state -> Transitions alph state
addTransition from sym to = M.alter addM from
  where addM (Just m) = Just $ M.alter addS sym m
        addM Nothing  = Just $ M.singleton sym $ S.singleton to
        addS (Just s) = Just $ S.insert to s
        addS Nothing  = Just $ S.singleton to

addKripkeTransition
    :: VarMapping -> StateId -> StateId
    -> Transitions SatisfiedVars StateId
    -> Either InterpretError (Transitions SatisfiedVars StateId)
addKripkeTransition vs s1 s2 trs = do
    sym <- satisfiedInEnv (snd s2) vs
    pure $ addTransition s1 sym s2 trs

-- | Given a bindings from propositional variables to expressions,
-- construct a Buchi automaton over 'SatisfiedVars' alphabet
evalToBuchi
    :: VarMapping
    -> EvalAutomaton
    -> Either InterpretError (BuchiAutomaton SatisfiedVars StateId)
evalToBuchi vs EvalAutomaton {..} = do
    let initStateId = (StmtId (-1) (-1), mempty :| [])
        -- ^ If there are no initial global vars declarations
        -- in program, will be the same as existing initial state
        eInits = S.filter (/= initStateId) _eaInit
        rootTrs = M.singleton initStateId mempty

    initTrs <- foldM (flip $ addKripkeTransition vs initStateId) rootTrs eInits
    let initVisited = S.singleton initStateId

        dfs :: (Set StateId, Transitions SatisfiedVars StateId)
            -> StateId
            -> Either InterpretError (Set StateId, Transitions SatisfiedVars StateId)
        dfs (visited, trans) sId = do
            let visited' = S.insert sId visited
                neighbors = fromMaybe (error "mda") $ M.lookup sId _eaTransitions
                newNeighbors = S.filter (not . flip S.member visited') neighbors
            trans' <- foldM (flip $ addKripkeTransition vs sId) trans neighbors
            foldM dfs (visited', trans') newNeighbors

    (baFinal, baTransitions) <- foldM dfs (initVisited, initTrs) eInits
    let baInit = S.singleton initStateId
    return BuchiAutomaton {..}
