
-- | Language interpreter

module Language.Interpret
       ( InterpretError (..)
       , Env
       , Automaton
       , IState (..)
       , iCurState
       , iAutomaton
       , evalExpr
       , evalStmt
       , runProgram
       , evalProgram
       , execProgram
       ) where

import Prelude (show)
import Universum hiding (Type, show)

import Control.Lens (makeLenses, (%=), (.=))
import Control.Monad.Except (catchError, throwError)
import qualified Data.Map.Strict as M

import Language.Decl (Decl (..), Program (..))
import Language.Expr (Expr, Ident, Op (..), Value (..))
import qualified Language.Expr as Expr
import Language.Stmt (Stmt, StmtId (..), Type (..))
import qualified Language.Stmt as Stmt

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "fromJust: Nothing"

--------------------------------------------------------------
-- Interpreter basics
--------------------------------------------------------------

data InterpretError
    = UndefinedVar !Ident
    | UndefinedFunc !Ident
    | UndeclaredVar !Ident
    | TypeMismatch !Type !Type
    | NoReturnVal !Ident
    | UnknownError !String
      -- * Special errors for control flow (way simpler than messing with ContT )00)
    | BreakError
    | ReturnError !(Maybe Value)
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
    show BreakError = "<break-error>"
    show (ReturnError _) = "<return-error>"

instance Exception InterpretError

instance {-# OVERLAPPING #-} Alternative (Either InterpretError) where
    a@(Right _) <|> _ = a
    Left _ <|> b = b
    empty = Left (UnknownError "")

type Scope = Map Ident (Type, Maybe Value)

scopeLookup :: Ident -> Scope -> Either InterpretError Value
scopeLookup x e = maybeToRight (UndefinedVar x) $
    M.lookup x e >>= snd

scopeDeclare :: Ident -> Type -> Scope -> Scope
scopeDeclare x t = M.insert x (t, Nothing)

valueType :: Value -> Type
valueType (Num _)     = Int'
valueType (Boolean _) = Bool'

scopeAssign :: Ident -> Value -> Scope -> Either InterpretError Scope
scopeAssign x v e =
    maybeToRight (UndeclaredVar x) (M.lookup x e) >>=
    \(t, _) -> if t == valueType v
               then Right $ M.insert x (t, Just v) e
               else Left $ TypeMismatch (valueType v) t

type Env = NonEmpty Scope

envLookup :: Ident -> Env -> Either InterpretError Value
envLookup x (g :| (l:ls)) = scopeLookup x l <|> envLookup x (g :| ls)
envLookup x (g :| [])     = scopeLookup x g

envDeclare :: Ident -> Type -> Env -> Env
envDeclare x t (g :| (l:ls)) = g :| (scopeDeclare x t l : ls)
envDeclare x t (g :| [])     = scopeDeclare x t g :| []

envAssign :: Ident -> Value -> Env -> Either InterpretError Env
envAssign x v (g :| ls) =
    (g :|) <$> scopeAssignLs x v ls <|>
    (:| ls) <$> scopeAssign x v g
  where
    scopeAssignLs x' _ [] = Left $ UndeclaredVar x'
    scopeAssignLs x' v' (e:es) =
        (:es) <$> scopeAssign x' v' e <|>
        (e:) <$> scopeAssignLs x' v' es

envEnter :: Env -> Env
envEnter (g :| ls) = g :| (mempty : ls)

envLeave :: Env -> Env
envLeave (g :| (_:ls)) = g :| ls
envLeave (_ :| [])     = error "impossible to leave the global scope"

type Decls = Map Ident Decl

type StateId = (StmtId, Env)

initStmtId :: StmtId
initStmtId = StmtId (-1) (-1)

type Automaton = Map StateId [StateId]

data IState = IState
    { _iCurState  :: !StateId
    , _iAutomaton :: !Automaton
    } deriving (Eq, Show, Generic)

makeLenses ''IState

iEnv :: Lens' IState Env
iEnv = iCurState . _2

type Interpreter = StateT IState
    (ReaderT Decls (Either InterpretError))

lookup :: Ident -> Interpreter Value
lookup x = use iEnv >>= either throwError pure . envLookup x

declare :: Ident -> Type -> Interpreter ()
declare x t = iEnv %= envDeclare x t

assign :: Ident -> Value -> Interpreter ()
assign x v = use iEnv >>= either throwError (iEnv .=) . envAssign x v

enter :: Interpreter ()
enter = iEnv %= envEnter

leave :: Interpreter ()
leave = iEnv %= envLeave

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
    enter
    forM_ (zip dParams args) $
        \((t, x), v) -> declare x t >> assign x v
    res <- evalFuncBody dBody
    leave
    when (fmap valueType res /= dReturnType) $
        throwError $ TypeMismatch (valueType $ fromJust res) (fromJust dReturnType)
    return res

-----------------------------------------------------------------
-- Statements evaluation
-----------------------------------------------------------------

evalStmt :: Stmt -> Interpreter (Maybe Value)
evalStmt (Stmt.Seq a b)      = evalStmt a >> evalStmt b
evalStmt (Stmt.Atomic _ s)   = evalAtomic s
evalStmt Stmt.Skip           = pure Nothing
evalStmt (Stmt.Declare t _ x)  = Nothing <$ declare x t
evalStmt (Stmt.Assign _ x e) = evalExpr e >>= \v -> Just v <$ assign x v
evalStmt (Stmt.If cond a b) =
    ifM (evalExpr cond >>= getBool) (evalScope a) (evalScope b)
evalStmt (Stmt.While cond s) =
    let loop = ifM (evalExpr cond >>= getBool) (evalScope s >> loop) (pure Nothing)
        catchBreak BreakError = pure Nothing
        catchBreak err        = throwError err
    in loop `catchError` catchBreak
evalStmt (Stmt.Break _) = throwError BreakError
evalStmt (Stmt.Return _ me) =
    traverse evalExpr me >>= throwError . ReturnError
evalStmt (Stmt.Call f args) = mapM evalExpr args >>= evalCall f

evalScope :: Stmt.Scope -> Interpreter (Maybe Value)
evalScope Stmt.Scope {..} = do
    enter
    res <- evalStmt scopeBody
    leave
    return res

-- | Later it would create one edge in automata instead of many
evalAtomic :: Stmt -> Interpreter (Maybe Value)
evalAtomic = evalStmt

evalFuncBody :: Stmt -> Interpreter (Maybe Value)
evalFuncBody st = evalStmt st `catchError` catchReturn
  where catchReturn (ReturnError me) = pure me
        catchReturn err              = throwError err

------------------------------------------------------------------
-- Program evaluation
------------------------------------------------------------------

runProgram :: Program -> Either InterpretError (Maybe Value, IState)
runProgram Program {..} =
    usingReaderT funcMap $ usingStateT initState $ evalFuncBody progMain
  where
    funcMap = foldl' (\m d -> M.insert (dName d) d m) mempty progFuncs
    initEnv = foldl' (\m (t, x, v) -> M.insert x (t, v) m) mempty progVars :| []
    initStateId = (initStmtId, initEnv)
    initAutomaton = M.singleton initStateId []
    initState = IState initStateId initAutomaton

evalProgram :: Program -> Either InterpretError (Maybe Value)
evalProgram = fmap fst . runProgram

execProgram :: Program -> Either InterpretError IState
execProgram = fmap snd . runProgram
