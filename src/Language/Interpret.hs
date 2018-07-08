
-- | Language interpreter

module Language.Interpret
       ( evalExpr
       , evalStmt
       , runProgram
       , evalProgram
       , execProgram
       ) where

import Prelude (show)
import Universum hiding (Type, show)

import Control.Monad.Except (catchError, liftEither, throwError)
import qualified Data.Map.Strict as M

import Language.Decl (Decl (..), Program (..), parseProgram)
import Language.Expr (Expr, Ident, Op (..), Value (..))
import qualified Language.Expr as Expr
import Language.Stmt (Stmt, Type (..))
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

emptyEnv :: Env
emptyEnv = mempty :| []

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

type Interpreter = StateT Env (ReaderT Decls (Either InterpretError))

lookup :: Ident -> Interpreter Value
lookup x = get >>= liftEither . envLookup x

declare :: Ident -> Type -> Interpreter ()
declare x = modify' . envDeclare x

assign :: Ident -> Value -> Interpreter ()
assign x v = get >>= either throwError put . envAssign x v

enter :: Interpreter ()
enter = modify' envEnter

leave :: Interpreter ()
leave = modify' envLeave

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

getInt :: Value -> Interpreter Int
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

arithOp :: (Int -> Int -> Int) -> Value -> Value -> Interpreter Value
arithOp f a b = Num <$> (f <$> getInt a <*> getInt b)

compOp :: (Int -> Int -> Bool) -> Value -> Value -> Interpreter Value
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
evalStmt (Stmt.Seq a b)     = evalStmt a >> evalStmt b
evalStmt (Stmt.Skip)        = pure Nothing
evalStmt (Stmt.Declare t x) = Nothing <$ declare x t
evalStmt (Stmt.Assign x e)  = evalExpr e >>= \v -> Just v <$ assign x v
evalStmt (Stmt.If cond a b) =
    ifM (evalExpr cond >>= getBool) (evalStmt a) (evalStmt b)
evalStmt (Stmt.While cond s) =
    let loop = ifM (evalExpr cond >>= getBool) (evalStmt s >> loop) (pure Nothing)
        catchBreak BreakError = pure Nothing
        catchBreak err        = throwError err
    in loop `catchError` catchBreak
evalStmt Stmt.Break = throwError BreakError
evalStmt (Stmt.Return me) =
    traverse evalExpr me >>= throwError . ReturnError
evalStmt (Stmt.Call f args) = mapM evalExpr args >>= evalCall f

evalFuncBody :: Stmt -> Interpreter (Maybe Value)
evalFuncBody st = evalStmt st `catchError` catchReturn
  where catchReturn (ReturnError me) = pure me
        catchReturn err              = throwError err

------------------------------------------------------------------
-- Program evaluation
------------------------------------------------------------------

runProgram :: Program -> Either InterpretError (Maybe Value, Env)
runProgram Program {..} =
    usingReaderT funcMap $ usingStateT emptyEnv $ evalFuncBody progMain
  where
    funcMap = foldl' (\m d -> M.insert (dName d) d m) mempty progFuncs

evalProgram :: Program -> Either InterpretError (Maybe Value)
evalProgram = fmap fst . runProgram

execProgram :: Program -> Either InterpretError Env
execProgram = fmap snd . runProgram
