{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv) where

import Control.Monad.Cont (callCC)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Eval (Env (..), Eval, EvalError (..), Value (..), apply, renderVal, showVal, IFunc (..))

unpackNum :: Value -> Eval Integer
unpackNum (VNumber n) = pure n
unpackNum v = throwError $ TypeError $ "expected number, got " <> showVal v

divOp :: [Value] -> Eval Value
divOp [lhs, rhs] = do
  lhs' <- unpackNum lhs
  rhs' <- unpackNum rhs
  if rhs' == 0 then throwError $ NumericError "division by zero"
  else pure $ VNumber $ lhs' `div` rhs'
divOp args = arityError 2 args

mkOp :: (Value -> Value -> Bool) -> [Value] -> Eval Value
mkOp op [lhs, rhs] = pure $ VBoolean $ lhs `op` rhs
mkOp _ args = arityError 2 args

arityError :: Int -> [Value] -> Eval Value
arityError expected args = throwError $ ArityError expected $ length args

cons :: [Value] -> Eval Value
cons [a, b] = pure $ VPair a b
cons args = arityError 2 args

car :: [Value] -> Eval Value
car [VPair a _] = pure a
car [v] = throwError $ TypeError $ "expected a pair, got: " <> showVal v
car args = arityError 1 args

cdr :: [Value] -> Eval Value
cdr [VPair _ b] = pure b
cdr [v] = throwError $ TypeError $ "expected a pair, got: " <> showVal v
cdr args = arityError 1 args

isNull :: [Value] -> Eval Value
isNull [v] = pure $ VBoolean $ case v of VNil -> True; _ -> False
isNull args = arityError 1 args

isNumber :: [Value] -> Eval Value
isNumber [v] = pure $ VBoolean $ case v of VNumber _ -> True; _ -> False
isNumber args = arityError 1 args

isBoolean :: [Value] -> Eval Value
isBoolean [v] = pure $ VBoolean $ case v of VBoolean _ -> True; _ -> False
isBoolean args = arityError 1 args

isSymbol :: [Value] -> Eval Value
isSymbol [v] = pure $ VBoolean $ case v of VSymbol _ -> True; _ -> False
isSymbol args = arityError 1 args

isPair :: [Value] -> Eval Value
isPair [v] = pure $ VBoolean $ case v of VPair {} -> True; _ -> False
isPair args = arityError 1 args

isProcedure :: [Value] -> Eval Value
isProcedure [v] = pure $ VBoolean $ case v of VProc {} -> True; _ -> False
isProcedure args = arityError 1 args

display :: [Value] -> Eval Value
display args = liftIO $ mapM_ (TIO.putStr . renderVal False) args >> pure VNil

write :: [Value] -> Eval Value
write args = liftIO $ mapM_ (TIO.putStr . renderVal True) args >> pure VNil

isEq :: [Value] -> Eval Value
isEq [a, b] = pure $ VBoolean $ case (a, b) of
  (VSymbol x, VSymbol y) -> x == y
  (VNumber x, VNumber y) -> x == y
  (VBoolean x, VBoolean y) -> x == y
  (VNil, VNil) -> True
  _ -> False
isEq args = arityError 2 args

mkBinaryNumOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
mkBinaryNumOp op [v1, v2] = do
  n1 <- unpackNum v1
  n2 <- unpackNum v2
  if n2 == 0
    then throwError $ NumericError "division by zero"
    else pure $ VNumber $ n1 `op` n2
mkBinaryNumOp _ args = arityError 2 args

callCC' :: [Value] -> Eval Value
callCC' [func@(VProc {})] = do
  callCC $ \k -> do
    let cont args = case args of
          [arg] -> k arg
          _ -> arityError 1 args
    apply func [VProc $ IFunc cont]
callCC' [v] = throwError $ TypeError $ "expected a function, got: " <> showVal v
callCC' args = arityError 1 args

primitives :: [(Text, [Value] -> Eval Value)]
primitives =
  [ ("+", mkBinaryNumOp (+)),
    ("-", mkBinaryNumOp (-)),
    ("*", mkBinaryNumOp (*)),
    ("/", divOp),
    ("quotient", mkBinaryNumOp quot),
    ("remainter", mkBinaryNumOp rem),
    ("modulo", mkBinaryNumOp mod),
    ("=", mkOp (==)),
    ("<", mkOp (<)),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr),
    ("number?", isNumber),
    ("boolean?", isBoolean),
    ("symbol?", isSymbol),
    ("procedure", isProcedure),
    ("null?", isNull),
    ("pair?", isPair),
    ("display!", display),
    ("write!", write),
    ("eq?", isEq),
    ("call/cc", callCC')
  ]

primEnv :: IO Env
primEnv = do
  entries <- mapM makeEntry primitives
  bindings <- newIORef $ Map.fromList entries
  pure $ Env {parent = Nothing, bindings}
  where
    makeEntry (name, func) = do
      cell <- newIORef (VProc $ IFunc func)
      pure (name, cell)
