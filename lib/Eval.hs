{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( eval,
    runEval,
    search,
    apply
  )
where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.IORef (readIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import SExpr (Cell, Env (..), Eval (..), EvalError (..), EvalResult, Op (..), SExpr (..), renderVal)

runEval :: Env -> Eval SExpr -> IO EvalResult
runEval env ev =
  runContT
    ( runExceptT
        (runReaderT (unEval ev) env)
    )
    pure

apply :: SExpr -> SExpr -> Eval SExpr
apply (SOp (Op op)) args = op args
apply op _ = throwError $ TypeError $ renderVal True op <> " not an operative"

search :: Text -> Eval Cell
search sym = do
  Env {parent, frame} <- ask
  bindings <- liftIO $ readIORef $ frame
  case Map.lookup sym bindings of
    Just val -> pure val
    Nothing -> case parent of
      Just p -> local (const p) $ search sym
      Nothing -> throwError $ RuntimeError $ "'" <> sym <> "' not found"

eval :: SExpr -> Eval SExpr
eval (SSym sym) = search sym >>= liftIO . readIORef
eval (SPair op args) = do
  op' <- eval op
  apply op' args
eval x = pure $ x
