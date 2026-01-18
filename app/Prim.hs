{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv) where

import Control.Monad.Reader (MonadIO (..), MonadReader (..))
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import Eval (currying, dispatch, eval, ternary, unary)
import SExpr (Env (..), Eval, Op (..), SExpr (..))

mkOp :: (SExpr -> Eval SExpr) -> SExpr
mkOp = SOp . Op

opOnPair :: (SExpr -> SExpr -> Eval SExpr) -> SExpr
opOnPair f = mkOp $ \args -> case args of
  SPair l r -> f l r
  _ -> pure SNil

mapP :: (SExpr -> Eval SExpr) -> SExpr -> Eval SExpr
mapP _ SNil = pure SNil
mapP op (SPair l r) = do
  l' <- op l
  r' <- mapP op r
  pure $ SPair l' r'
mapP op pair =
  dispatch pair $
    fold
      [ SNil,
        opOnPair $ \l r -> do
          l' <- op l
          r' <- mapP op r
          pure $ SPair l' r'
      ]

eager :: SExpr -> SExpr
eager op =
  mkOp
    ( \args -> do
        args' <- mapP eval args
        dispatch op args'
    )

fold :: [SExpr] -> SExpr
fold = foldr SPair SNil

bindAndEval :: SExpr -> SExpr -> SExpr -> Eval SExpr
bindAndEval (SPair p ps) args expr =
  dispatch args $
    fold
      [ mkOp $ \as -> bindAndEval ps as expr,
        opOnPair $ \a as -> do
          _ <-
            dispatch p $
              fold
                [ fold
                    [mkOp $ currying 1 $ unary $ \ast -> pure ast, a]
                ]
          bindAndEval ps as expr
      ]
bindAndEval SNil args expr =
  dispatch args $
    fold
      [ expr,
        mkOp $ \_ -> do
          val <- eval expr
          dispatch val args
      ]
bindAndEval sym args expr = do
  _ <- dispatch sym (fold [fold [SSym "quote", args]])
  eval expr

vau :: SExpr -> SExpr -> SExpr -> Eval SExpr
vau params envParam expr = do
  parent <- ask
  pure $ mkOp $ \args -> do
    frame <- liftIO $ newIORef $ Map.empty
    dynEnv <- ask
    let env = Env (Just parent) frame
    let envOp = eager $
          mkOp $
            currying 1 $
              unary $
                \ast -> do
                  local (const dynEnv) $ eval ast
    local (const env) $ do
      _ <- dispatch envParam envOp
      bindAndEval params args expr

car :: SExpr -> Eval SExpr
car (SPair l _) = pure l
car SNil = pure SNil
car pair =
  dispatch pair $
    fold [SNil, opOnPair $ \l _ -> pure l]

cdr :: SExpr -> Eval SExpr
cdr (SPair _ r) = pure r
cdr SNil = pure SNil
cdr pair =
  dispatch pair $
    fold [SNil, opOnPair $ \_ r -> pure r]

primitives :: [(Text, SExpr)]
primitives =
  [ ("quote", mkOp $ currying 1 $ unary $ \ast -> pure ast),
    ("true", SBool True),
    ("false", SBool False),
    ("car", eager $ mkOp $ currying 1 $ unary car),
    ("cdr", eager $ mkOp $ currying 1 $ unary cdr),
    ("$", mkOp $ currying 3 $ ternary vau),
    ("eager", eager $ mkOp $ currying 1 $ unary $ pure . eager)
  ]

primEnv :: IO Env
primEnv = do
  entries <- traverse makeEntry primitives
  frame <- newIORef $ Map.fromList entries
  pure $ Env {parent = Nothing, frame}
  where
    makeEntry (name, val) = do
      cell <- newIORef val
      pure (name, cell)
