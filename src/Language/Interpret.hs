
-- | Language interpreter

module Language.Interpret
       ( InterpretError (..)
       , Interpreter
       , Env
       , StateId
       , EvalAutomaton (..)
       , eaTransitions
       , eaInit
       , IState (..)
       , evalExpr
       , evalStmt
       , runProgram
       , evalProgram
       , execProgram
       ) where

import Prelude (show)
import Universum hiding (Type, show)

import Control.Lens (makeLenses, uses, (%=), (+=), (-=), (.=))
import Control.Monad.Except (catchError, throwError)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.List ((!!))
import Language.Decl (Decl (..), Program (..))
import Language.Expr (Expr, Ident, Op (..), Value (..))
import qualified Language.Expr as Expr
import Language.Stmt (Stmt, StmtId (..), Type (..))
import qualified Language.Stmt as Stmt

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "fromJust: Nothing"

--------------------------------------------------------------
-- Interpreter scope handling
--------------------------------------------------------------

-- | Interpreter errors
data InterpretError
    = UndefinedVar !Ident
    | UndefinedFunc !Ident
    | UndeclaredVar !Ident
    | TypeMismatch !Type !Type
    | NoReturnVal !Ident
    | UnknownError !String
      -- * Special errors for control flow (way simpler than messing with ContT )00)
    | InfiniteLoop
    | BreakError !StmtId !Int
    | ReturnError !StmtId !Int !(Maybe Value)
    deriving (Eq, Generic)

instance Show InterpretError where
    show (UndefinedVar x) =
        "Undefined variable: " ++ show x
    show (UndefinedFunc f) =
        "Undefined function: " ++ show f
    show (UndeclaredVar x) =
        "Trying to assign a value to variable '" ++
        show x ++ "', which hasn't been declared before"
    show (TypeMismatch actual expected) =
        "Type mismatch: expected value of type '" ++ show expected ++
        "', but got a value of type '" ++ show actual ++ "'"
    show (NoReturnVal f) =
        "Function '" ++ show f ++ "' was expected to return a value"
    show (UnknownError s) =
        "Unknown error: " ++ s
    show InfiniteLoop = "<infinite-loop>"
    show BreakError{} = "<break-error>"
    show ReturnError{} = "<return-error>"

instance Exception InterpretError

-- | Necessary to be able to use '<|>' combinator
instance {-# OVERLAPPING #-} Alternative (Either InterpretError) where
    a@(Right _) <|> _ = a
    Left _ <|> b = b
    empty = Left (UnknownError "")

-- | A single scope
type Scope = Map Ident (Type, Maybe Value)

scopeLookup :: Ident -> Scope -> Either InterpretError Value
scopeLookup x e = maybeToRight (UndefinedVar x) $
    M.lookup x e >>= snd

scopeDeclare :: Ident -> Type -> Scope -> Scope
scopeDeclare x t = M.insert x (t, Nothing)

valueType :: Value -> Type
valueType (Num _)     = Int'
valueType (Boolean _) = Bool'

-- | Assignment of value to an undeclared variable is prohibited
scopeAssign :: Ident -> Value -> Scope -> Either InterpretError Scope
scopeAssign x v e =
    maybeToRight (UndeclaredVar x) (M.lookup x e) >>=
    \(t, _) -> if t == valueType v
               then Right $ M.insert x (t, Just v) e
               else Left $ TypeMismatch (valueType v) t

-- | A stack of scopes
type Env = NonEmpty Scope

-- | Looking for a variable down the stack
envLookup :: Ident -> Env -> Either InterpretError Value
envLookup x (g :| (l:ls)) = scopeLookup x l <|> envLookup x (g :| ls)
envLookup x (g :| [])     = scopeLookup x g

-- | Declare a variable in current local scope
envDeclare :: Ident -> Type -> Env -> Env
envDeclare x t (g :| (l:ls)) = g :| (scopeDeclare x t l : ls)
envDeclare x t (g :| [])     = scopeDeclare x t g :| []

-- | Assigns a value to a declared variable, looking it up in
-- scope stack
envAssign :: Ident -> Value -> Env -> Either InterpretError Env
envAssign x v (g :| ls) =
    ((g :|) <$> scopeAssignLs x v ls) `orIfUndeclared`
    ((:| ls) <$> scopeAssign x v g)
  where
    scopeAssignLs :: Ident -> Value -> [Scope] -> Either InterpretError [Scope]
    scopeAssignLs x' _ [] = Left $ UndeclaredVar x'
    scopeAssignLs x' v' (e:es) =
        ((:es) <$> scopeAssign x' v' e) `orIfUndeclared`
        ((e:) <$> scopeAssignLs x' v' es)

    -- Cannot simply use '<|>', because this way if there's shadowed variable with
    -- the same name and another type deeper in the stack and we go deeper on `TypeMismatch`,
    -- this shadowed variable will be silently updated.
    orIfUndeclared ::
        Either InterpretError a -> Either InterpretError a -> Either InterpretError a
    orIfUndeclared (Left (UndeclaredVar _)) b = b
    orIfUndeclared a _                        = a

envEnter :: Env -> Env
envEnter (g :| ls) = g :| (mempty : ls)

envLeave :: Env -> Env
envLeave (g :| (_:ls)) = g :| ls
envLeave (_ :| [])     = error "impossible to leave the global scope"

envDepth :: Env -> Int
envDepth = length

envLeaveN :: Int -> Env -> Env
envLeaveN 0 = identity
envLeaveN d = (!! (d-1)) . iterate envLeave

----------------------------------------------------------
-- Interpreter stateful monad
----------------------------------------------------------

-- | Map of function declarations
type Decls = Map Ident Decl

-- | State is indentified by a current scope stack and
-- id of instruction which caused the change in scope stack
type StateId = (StmtId, Env)

initStmtId :: StmtId
initStmtId = StmtId (-1) (-1)

-- | EvalAutomaton (Kripke structure) for a program
data EvalAutomaton = EvalAutomaton
    { _eaTransitions :: !(Map StateId (Set StateId))
    , _eaInit        :: !(Set StateId)
    } deriving (Eq, Show, Generic)

makeLenses ''EvalAutomaton

addEdge :: StateId -> StateId -> EvalAutomaton -> EvalAutomaton
addEdge from to = eaTransitions %~ M.alter (add to) from
  where add e (Just es) = Just $ S.insert e es
        add e Nothing   = Just $ S.singleton e

hasEdge :: StateId -> StateId -> EvalAutomaton -> Bool
hasEdge from to =
    maybe False (S.member to) . M.lookup from . _eaTransitions

-- | Interpreter state
data IState = IState
    { _iCurState  :: !StateId
      -- ^ Current program state
    , _iAutomaton :: !EvalAutomaton
      -- ^ EvalAutomaton which has been built so far
    , _iAtomicD   :: !Int
      -- ^ If >0, then current code is executed atomically (all computations
      -- represent a single edge in Kripke model).
    } deriving (Eq, Show, Generic)

makeLenses ''IState

iEnv :: Lens' IState Env
iEnv = iCurState . _2

iStmtId :: Lens' IState StmtId
iStmtId = iCurState . _1

-- | Interpreter monad
type Interpreter = StateT IState
    (ReaderT Decls (Either InterpretError))

-- | Perform an action which changes the state with
-- constructing the corresponding edge in Kripke graph
transition :: StmtId -> Interpreter a -> Interpreter a
transition sId action = do
    atomicD <- use iAtomicD
    initState <- use iCurState
    when (atomicD <= 0) $
        iStmtId .= sId
    res <- action
    when (atomicD <= 0) $ do
        resState <- use iCurState
        edgeExists <- uses iAutomaton (hasEdge initState resState)
        when edgeExists $
            throwError InfiniteLoop
        iAutomaton %= addEdge initState resState
    return res

-- | Perform an action atomically (there will be only
-- one edge constructed even if there are transitions inside this action).
atomic :: StmtId -> Interpreter a -> Interpreter a
atomic sId action = transition sId $ do
    iAtomicD += 1
    res <- action
    iAtomicD -= 1
    return res

-- Monadic scope manipulation

depth :: Interpreter Int
depth = uses iEnv envDepth

lookup :: Ident -> Interpreter Value
lookup x = use iEnv >>= either throwError pure . envLookup x

declare :: StmtId -> Ident -> Type -> Interpreter ()
declare sId x t = transition sId $ iEnv %= envDeclare x t

assign :: StmtId -> Ident -> Value -> Interpreter ()
assign sId x v = transition sId $
    use iEnv >>= either throwError (iEnv .=) . envAssign x v

enter :: StmtId -> Interpreter ()
enter sId = transition sId $ iEnv %= envEnter

leave :: StmtId -> Interpreter ()
leave sId = transition sId $ iEnv %= envLeave

leaveN :: StmtId -> Int -> Interpreter ()
leaveN sId n = transition sId $ iEnv %= envLeaveN n

-------------------------------------------------------------
-- Expressions evaluation
-------------------------------------------------------------

evalExpr :: Expr -> Interpreter Value
evalExpr (Expr.Const v)      = pure v
evalExpr (Expr.Var x)        = lookup x
evalExpr (Expr.Binop op a b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr (Expr.Call f args) =
    (mapM evalExpr args >>= evalCall f)
    `whenNothingM` throwError (NoReturnVal f)

getInt :: Value -> Interpreter Word8
getInt (Num i) = pure i
getInt _       = throwError $ TypeMismatch Bool' Int'

getBool :: Value -> Interpreter Bool
getBool (Boolean b) = pure b
getBool _           = throwError $ TypeMismatch Int' Bool'

evalOp :: Op -> Value -> Value -> Interpreter Value
evalOp Add  = arithOp (+)
evalOp Sub  = arithOp (-)
evalOp Mul  = arithOp (*)
evalOp Div  = arithOp div
evalOp Mod  = arithOp mod
evalOp Eq   = compOp (==)
evalOp NEq  = compOp (/=)
evalOp LEq  = compOp (<=)
evalOp GEq  = compOp (>=)
evalOp Less = compOp (<)
evalOp More = compOp (>)
evalOp And  = boolOp (&&)
evalOp Or   = boolOp (||)

arithOp :: (Word8 -> Word8 -> Word8) -> Value -> Value -> Interpreter Value
arithOp f a b = Num <$> (f <$> getInt a <*> getInt b)

compOp :: (Word8 -> Word8 -> Bool) -> Value -> Value -> Interpreter Value
compOp f a b = Boolean <$> (f <$> getInt a <*> getInt b)

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Interpreter Value
boolOp f a b = Boolean <$> (f <$> getBool a <*> getBool b)

evalCall :: Ident -> [Value] -> Interpreter (Maybe Value)
evalCall f args = do
    Decl {..} <- asks (M.lookup f) `whenNothingM` throwError (UndefinedFunc f)
    let Stmt.Scope {..} = dBody
    atomic scopeBegin $ do
        enter scopeBegin
        forM_ (zip dParams args) $
            \((t, x), v) -> declare initStmtId x t >> assign initStmtId x v
    initDepth <- depth
    res <- evalFuncBody initDepth scopeBody
    leave scopeEnd
    when (fmap valueType res /= dReturnType) $
        throwError $ TypeMismatch (valueType $ fromJust res) (fromJust dReturnType)
    return res

-----------------------------------------------------------------
-- Statements evaluation
-----------------------------------------------------------------

evalStmt :: Stmt -> Interpreter (Maybe Value)
evalStmt (Stmt.Seq a b)         = evalStmt a >> evalStmt b
evalStmt (Stmt.Atomic sId s)    = atomic sId $ evalStmt s
evalStmt Stmt.Skip              = pure Nothing
evalStmt (Stmt.Declare t sId x) = Nothing <$ declare sId x t
evalStmt (Stmt.Assign sId x e)  = evalExpr e >>= \v -> Just v <$ assign sId x v
evalStmt (Stmt.If cond a b) =
    ifM (evalExpr cond >>= getBool) (evalScope a) (evalScope b)
evalStmt (Stmt.While cond s) = do
    let loop = ifM (evalExpr cond >>= getBool) (evalScope s >> loop) (pure Nothing)
        catchBreak d (BreakError sId d') = leaveN sId (d' - d) >> pure Nothing
        catchBreak _ err                 = throwError err
    initDepth <- depth
    loop `catchError` catchBreak initDepth
evalStmt (Stmt.Break sId) = do
    d <- depth
    throwError $ BreakError sId d
evalStmt (Stmt.Return sId me) = do
    mv <- traverse evalExpr me
    d <- depth
    throwError $ ReturnError sId d mv
evalStmt (Stmt.Call f args) = mapM evalExpr args >>= evalCall f

evalScope :: Stmt.Scope -> Interpreter (Maybe Value)
evalScope Stmt.Scope {..} = do
    enter scopeBegin
    res <- evalStmt scopeBody
    leave scopeEnd
    return res

evalFuncBody :: Int -> Stmt -> Interpreter (Maybe Value)
evalFuncBody dp st = evalStmt st `catchError` catchReturn dp
  where catchReturn d (ReturnError sId d' mv) = leaveN sId (d' - d) >> pure mv
        catchReturn _ err                     = throwError err

------------------------------------------------------------------
-- Program evaluation
------------------------------------------------------------------

runProgram :: Program -> Either InterpretError (Maybe Value, IState)
runProgram Program {..} =
    usingReaderT funcMap $ usingStateT initState $
    (evalFuncBody 1 progMain <* finalLoop) `catchError` catchLoop
  where
    funcMap = foldl' (\m d -> M.insert (dName d) d m) mempty progFuncs
    initEnv = foldl' (\m (t, x, v) -> M.insert x (t, v) m) mempty progVars :| []
    initStateId = (initStmtId, initEnv)
    initAutomaton = EvalAutomaton
        { _eaTransitions = M.singleton initStateId mempty
        , _eaInit = S.singleton initStateId
        }
    initState = IState initStateId initAutomaton 0

    -- Make the loop in final state of terminating program
    finalLoop = do
        curState <- use iCurState
        iAutomaton %= addEdge curState curState

    -- Handle the case of non-terminating program
    catchLoop InfiniteLoop = pure Nothing
    catchLoop err          = throwError err

evalProgram :: Program -> Either InterpretError (Maybe Value)
evalProgram = fmap fst . runProgram

execProgram :: Program -> Either InterpretError IState
execProgram = fmap snd . runProgram
