{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( eval,
    runEval,
    apply,
    showVal,
    renderVal,
    printError,
  )
where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SExpr (Cell, Env (..), Eval (..), EvalError (..), IFunc (..), SExpr (..))

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

runEval :: Env -> Eval SExpr -> IO (Either EvalError SExpr)
runEval env ev =
  runContT
    (runExceptT (runReaderT (unEval ev) env))
    pure

renderVal :: Bool -> SExpr -> Text
renderVal _ (SNumber n) = T.pack $ show n
renderVal True (SString s) = "\"" <> s <> "\""
renderVal False (SString s) = s
renderVal _ SNil = "()"
renderVal _ (SBoolean True) = "#t"
renderVal _ (SBoolean False) = "#f"
renderVal _ (SSymbol s) = s
renderVal _ (SProc {}) = "#<procedure>"
renderVal r (SPair l r') =
  let showTail SNil = ""
      showTail (SPair left right) = " " <> renderVal r left <> showTail right
      showTail right = " . " <> renderVal r right
   in "(" <> renderVal r l <> showTail r' <> ")"
renderVal _ (SMacro {}) = "#<macro>"

showVal :: SExpr -> Text
showVal = renderVal True

flatten :: SExpr -> Eval [SExpr]
flatten SNil = pure []
flatten (SPair l r) = (l :) <$> flatten r
flatten _ = throwError $ SyntaxError "cannot flatten an improper list"

evalMacro :: Text -> SExpr -> Eval SExpr
evalMacro param expr = do
  env <- ask
  let macro args = do
        cell <- liftIO $ newIORef args
        frame <- liftIO $ newIORef $ Map.fromList [(param, cell)]
        let env' = Env (Just env) frame
        expanded <- local (const env') $ eval expr
        eval expanded
  pure $ SMacro $ IFunc macro

evalIf :: SExpr -> SExpr -> SExpr -> Eval SExpr
evalIf pre con alt = do
  res <- eval pre
  case res of
    SBoolean False -> eval alt
    _ -> eval con

evalSet :: Text -> SExpr -> Eval SExpr
evalSet name val = do
  cell <- getVar name
  liftIO $ modifyIORef' cell $ const val
  pure val

evalDo :: SExpr -> Eval SExpr
evalDo (SNil) = pure $ SNil
evalDo (SPair l SNil) = eval l
evalDo (SPair l r) = eval l >> evalDo r
evalDo _ = throwError $ SyntaxError "cannot evaluate an improper list"

evalDefine :: Text -> SExpr -> Eval SExpr
evalDefine name val = do
  Env {bindings} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' bindings $ Map.insert name cell
  pure val

evalLambda :: SExpr -> SExpr -> Eval SExpr
evalLambda params expr = do
  params' <- flatten params
  env <- ask
  let func args = do
        args' <- mapM (liftIO . newIORef . eval) args
        bindings <- liftIO $ newIORef $ Map.fromList $ zip params' args'
        let env' = Env (Just env) bindings
        local (const env') $ eval expr
  pure (SProc $ IFunc func)

apply :: SExpr -> [SExpr] -> Eval SExpr
apply (SProc f) args = fn f args
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

eval :: SExpr -> Eval SExpr
eval (SSymbol s) = getVar s >>= liftIO . readIORef
eval (SPair (SSymbol "if") (SPair pre (SPair con (SPair alt SNil)))) =
  evalIf pre con alt
eval (SPair (SSymbol "quote") (SPair x SNil)) = pure $ datumToSExpr x
eval (SPair (SSymbol "lambda") (SPair params@(SPair {}) (SPair body SNil))) = do
  evalLambda params body
eval (SPair (SSymbol "do") body) = evalDo body
eval (SPair (SSymbol "define!") (SPair (SSymbol name) (SPair body SNil))) =
  eval body >>= evalDefine name
eval (SPair (SSymbol "define!") (SPair (SPair (SSymbol name) params) (SPair body SNil))) =
  evalLambda params body >>= evalDefine name
eval (SPair (SSymbol "set!") (SPair (SSymbol name) (SPair body SNil))) =
  eval body >>= evalSet name
eval (SPair (SSymbol "macro") (SPair (SSymbol param) (SPair body SNil))) =
  evalMacro param body
eval (SPair fExpr args) = do
  fVal <- eval fExpr
  case fVal of
    SMacro {} -> do
      let args' = datumToSExpr args
      expanded <- applyMacro fVal args'
      datum <- valueToDatum expanded
      eval datum
    _ -> do
      values <- evalList args
      apply fVal values
eval x = pure $ datumToSExpr x
