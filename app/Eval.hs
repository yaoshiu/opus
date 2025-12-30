{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( Value (..),
    EvalError (..),
    Env (..),
    Eval (..),
    eval,
    runEval,
    apply,
    showVal,
    renderVal,
    printError,
  )
where

import Control.Monad (foldM)
import Control.Monad.Cont (ContT (..), MonadCont)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (SExpr (..))

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VCont (Value -> Eval Value)
  | VFunc [Text] SExpr Env
  | VPair Value Value
  | VNil
  | VMacro Text SExpr Env

data EvalError
  = UnboundVariable Text
  | TypeError Text
  | ArityError Int Int
  | SyntaxError Text
  | NumericError Text

type EvalResult = Either EvalError Value

type Cell = IORef Value

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, bindings :: Frame}

newtype Eval a = Eval {unEval :: ReaderT Env (ExceptT EvalError (ContT EvalResult IO)) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadError EvalError,
      MonadCont
    )

printError :: EvalError -> IO ()
printError err =
  TIO.putStrLn $
    case err of
      UnboundVariable s -> "unbound variable: " <> s
      TypeError m -> "type error: " <> m
      ArityError expected got ->
        "arity mismatch: expected "
          <> T.show expected
          <> " argument(s), but got "
          <> T.show got
      SyntaxError m -> "syntax error: " <> m
      NumericError m -> "numeric error: " <> m

runEval :: Env -> Eval Value -> IO (Either EvalError Value)
runEval env ev =
  runContT
    (runExceptT (runReaderT (unEval ev) env))
    pure

showSExpr :: SExpr -> Text
showSExpr (PBoolean False) = "#f"
showSExpr (PBoolean True) = "#t"
showSExpr pair@(PPair {}) =
  let showPair pair =
        case pair of
          PPair l PNil -> showSExpr l
          PPair l r@(PPair {}) -> showSExpr l <> showPair r
          PPair l r -> showSExpr l <> " . " <> showSExpr r
   in "(" <> showPair pair <> ")"
showSExpr (PNumber n) = T.show n
showSExpr (PString s) = "\"" <> s <> "\""
showSExpr (PSymbol s) = s

renderVal :: Bool -> Value -> Text
renderVal _ (VNumber n) = T.pack $ show n
renderVal True (VString s) = "\"" <> s <> "\""
renderVal False (VString s) = s
renderVal _ VNil = "()"
renderVal _ (VBoolean True) = "#t"
renderVal _ (VBoolean False) = "#f"
renderVal _ (VSymbol s) = s
renderVal _ (VPrim {}) = "#<primitive>"
renderVal _ (VFunc {}) = "#<procedure>"
renderVal _ (VCont {}) = "#<continuation>"
renderVal r (VPair l r') =
  let showTail VNil = ""
      showTail (VPair left right) = " " <> renderVal r left <> showTail right
      showTail right = " . " <> renderVal r right
   in "(" <> renderVal r l <> showTail r' <> ")"
renderVal _ (VMacro {}) = "#<macro>"

showVal :: Value -> Text
showVal = renderVal True

flatten :: SExpr -> [SExpr]
flatten PNil = []
flatten (PPair h t) = h : flatten t
flatten _ = error "Syntax Error: Improper list in special form"

datumToValue :: SExpr -> Value
datumToValue (PNumber n) = VNumber n
datumToValue (PBoolean b) = VBoolean b
datumToValue (PString s) = VString s
datumToValue (PSymbol s) = VSymbol s
datumToValue (PPair l r) = VPair (datumToValue l) (datumToValue r)
datumToValue PNil = VNil

eval :: SExpr -> Eval Value
eval (PSymbol s) = getVar s >>= liftIO . readIORef
eval expr@(PPair (PSymbol "if") _) = case flatten expr of
  [_, pre, con, alt] -> evalIf pre con alt
  _ -> throwError $ SyntaxError "invalid if syntax"
eval (PPair "quote" x) = pure $ datumToValue x
eval expr@(PPair (PSymbol "lambda") _) = case flatten expr of
  [_, params@(PPair{}), body] -> evalLambda (flatten params) body
  _ -> throwError $ SyntaxError "invalid lambda syntax"
eval (PPair (PSymbol "do") body) = evalDo $ flatten body 
eval expr@(PPair (PSymbol "define!") _) = case flatten expr of
  [_, PSymbol name, body] -> eval body >>= evalDefine name
  [_, PPair name params, body] -> eval $ PPair (PSymbol "define!") $ PPair name $ PPair (PSymbol "lambda") $ PPair params body
  _ -> throwError $ SyntaxError "invalid define! syntax"
eval expr@(PPair (PSymbol "set!") _) = case flatten expr of
  [_, PSymbol name, body] -> eval body >>= evalSet name
  _ -> throwError $ SyntaxError "invalid set! syntax"
eval expr@(PPair (PSymbol "macro") _) = case flatten expr of
  [_, PSymbol param, body] -> evalMacro param body
  _ -> throwError $ SyntaxError "invalid macro syntax"
eval (PPair fExpr args) = do
  fVal <- eval fExpr
  case fVal of
    VMacro {} -> do
      let args' = datumToValue args
      expanded <- applyMacro fVal args'
      eval (valueToDatum expanded)
    _ -> do
      values <- evalList args
      apply fVal values
eval x = pure $ datumToValue x

evalList :: SExpr -> Eval [Value]
evalList PNil = pure []
evalList (PPair h t) = (:) <$> eval h <*> evalList t
evalList _ = throwError $ SyntaxError "cannot evaluate an improper list"

applyMacro :: Value -> Value -> Eval Value
applyMacro (VMacro param expr env) args = do
  cell <- liftIO $ newIORef args
  frame <- liftIO $ newIORef $ Map.fromList [(param, cell)]
  let env' = Env (Just env) frame
  local (const env') $ eval expr
applyMacro _ _ = throwError $ SyntaxError "invalid macro application"

valueToDatum :: Value -> SExpr
valueToDatum (VNumber n) = PNumber n
valueToDatum (VBoolean b) = PBoolean b
valueToDatum (VString s) = PString s
valueToDatum (VSymbol s) = PSymbol s
valueToDatum VNil = PList []
valueToDatum (VPair l r) =
  let flatten (VPair h t) = h : flatten t
      flatten VNil = []
      flatten lastVal = [lastVal]
      isProper VNil = True
      isProper (VPair _ t) = isProper t
      isProper _ = False
   in if isProper (VPair l r)
        then PList (map valueToDatum (flatten (VPair l r)))
        else
          let xs = flatten (VPair l r)
           in PDotted (map valueToDatum (init xs)) (valueToDatum (last xs))
valueToDatum (VFunc {}) = error "Macros cannot return functions"
valueToDatum (VPrim {}) = error "Macros cannot return primitives"
valueToDatum (VCont {}) = error "Macros cannot return continuations"
valueToDatum (VMacro {}) = error "Macros cannot return macros"

evalMacro :: Text -> SExpr -> Eval Value
evalMacro param expr = ask >>= pure . VMacro param expr

evalIf :: SExpr -> SExpr -> SExpr -> Eval Value
evalIf pre con alt = do
  res <- eval pre
  case res of
    VBoolean False -> eval alt
    _ -> eval con

evalSet :: Text -> Value -> Eval Value
evalSet name val = do
  cell <- getVar name
  liftIO $ modifyIORef' cell $ const val
  pure val

evalDo :: [SExpr] -> Eval Value
evalDo body = foldM (const eval) VNil body

evalDefine :: Text -> Value -> Eval Value
evalDefine name val = do
  Env {bindings} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' bindings $ Map.insert name cell
  pure val

evalLambda :: [SExpr] -> SExpr -> Eval Value
evalLambda params expr = do
  names <- mapM getParam params
  env <- ask
  pure (VFunc names expr env)

getParam :: SExpr -> Eval Text
getParam (PSymbol s) = pure s
getParam bad = throwError $ SyntaxError $ "invalid parameter: " <> showSExpr bad

apply :: Value -> [Value] -> Eval Value
apply (VPrim f) args = f args
apply (VFunc params expr env) args
  | length params /= length args =
      throwError $ ArityError (length params) (length args)
  | otherwise = do
      args' <- mapM (liftIO . newIORef) args
      bindings <- liftIO $ newIORef $ Map.fromList $ zip params args'
      let env' = Env (Just env) bindings
      local (const env') $ eval expr
apply (VCont k) [val] = k val
apply (VCont _) args = throwError $ ArityError 1 $ length args
apply v _ = throwError $ TypeError $ "not a function: " <> showVal v

getVar :: Text -> Eval Cell
getVar s = do
  env <- ask
  cell <- liftIO $ lookFor s env
  maybe
    (throwError $ UnboundVariable s)
    pure
    cell
  where
    lookFor :: Text -> Env -> IO (Maybe Cell)
    lookFor name Env {parent, bindings} = do
      frame <- readIORef bindings
      maybe
        (maybe (pure Nothing) (lookFor name) parent)
        (pure . Just)
        $ Map.lookup name frame
