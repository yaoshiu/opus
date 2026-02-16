{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Prim (primEnv, unary) where

import Control.Monad.Cont (MonadCont (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadIO (..), MonadReader (..), local)
import Data.IORef (modifyIORef, newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Eval (apply, eval, search)
import Parser (single)
import SExpr (Env (..), Eval, EvalError (..), Op (..), SExpr (..), renderVal)
import Text.Megaparsec (errorBundlePretty, parse)

len :: SExpr -> Eval Int
len SNil = pure 0
len (SPair _ r) = pure . (1 +) =<< len r
len _ = throwError $ TypeError "improper list"

unary :: (SExpr -> Eval SExpr) -> SExpr -> Eval SExpr
unary op (SPair arg SNil) = op arg
unary _ args = throwError . ArityError 1 =<< len args

binary :: (SExpr -> SExpr -> Eval SExpr) -> SExpr -> Eval SExpr
binary op (SPair l (SPair r SNil)) = op l r
binary _ args = throwError . ArityError 2 =<< len args

ternary :: (SExpr -> SExpr -> SExpr -> Eval SExpr) -> SExpr -> Eval SExpr
ternary op (SPair a1 (SPair a2 (SPair a3 SNil))) = op a1 a2 a3
ternary _ args = throwError . ArityError 3 =<< len args

define :: SExpr -> SExpr -> Eval SExpr
define (SSym "_") val = pure val
define (SSym sym) val = do
  Env {frame} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef frame $ Map.insert sym cell
  pure val
define sym _ = throwError $ TypeError $ renderVal True sym <> " not a symbol"

set :: SExpr -> SExpr -> Eval SExpr
set (SSym sym) val = do
  cell <- search sym
  liftIO $ modifyIORef cell $ const val
  pure val
set sym _ = throwError $ TypeError $ renderVal True sym <> " not a symbol"

if' :: SExpr -> SExpr -> SExpr -> Eval SExpr
if' cond true false = do
  cond' <- eval cond
  case cond' of
    SBool False -> eval false
    _ -> eval true

bind :: SExpr -> SExpr -> Eval (Int, Int)
bind SNil SNil = pure (0, 0)
bind SNil args = pure . (0,) =<< len args
bind (SPair pl pr) args = case args of
  SNil -> len pr >>= pure . (,0) . (+ 1)
  (SPair al ar) -> do
    _ <- define pl al
    (l, r) <- bind pr ar
    pure (l + 1, r + 1)
  _ -> throwError $ TypeError "improper list"
bind params args = define params args >> pure (1, 1)

wrap :: SExpr -> SExpr
wrap op = SOp $ Op $ \args -> evalArgs args >>= apply op
  where
    evalArgs args = case args of
      SNil -> pure SNil
      SPair l r -> do
        l' <- eval l
        r' <- evalArgs r
        pure $ SPair l' r'
      _ -> throwError $ TypeError "improper list"

vau :: SExpr -> SExpr -> SExpr -> Eval SExpr
vau params envParam expr = do
  staticParent <- ask
  pure $ SOp $ Op $ \args -> do
    callerEnv <- ask
    localFrame <- liftIO $ newIORef Map.empty
    let localEnv = Env (Just staticParent) localFrame
    let envOp = wrap $ SOp $ Op $ unary $ \ast -> local (const callerEnv) $ eval ast
    local (const localEnv) $ do
      (l, r) <- bind params args
      if l /= r
        then
          throwError $ ArityError l r
        else
          pure ()
      _ <- define envParam envOp
      eval expr

plus :: SExpr -> SExpr -> Eval SExpr
plus (SNum l) (SNum r) = pure $ SNum $ l + r
plus _ _ = throwError $ TypeError $ "expect 2 numbers, got "

neg :: SExpr -> Eval SExpr
neg (SNum a) = pure $ SNum $ negate a
neg _ = throwError $ TypeError "expect 1 number"

times :: SExpr -> SExpr -> Eval SExpr
times (SNum l) (SNum r) = pure $ SNum $ l * r
times _ _ = throwError $ TypeError "expect 2 numbers"

divide :: SExpr -> SExpr -> Eval SExpr
divide (SNum l) (SNum r) =
  if r == 0
    then
      throwError $ RuntimeError "divide by zero"
    else pure $ SNum $ l `div` r
divide _ _ = throwError $ TypeError "expect 2 numbers"

cons :: SExpr -> SExpr -> Eval SExpr
cons a b = pure $ SPair a b

display :: SExpr -> Eval SExpr
display arg = do
  liftIO $ TIO.putStrLn $ renderVal False arg
  pure SNil

show' :: SExpr -> Eval SExpr
show' arg = pure $ foldr (SPair . SChar) SNil $ T.unpack $ renderVal False arg

eq :: SExpr -> SExpr -> Eval SExpr
eq a b = pure $ SBool $ a == b

lt :: SExpr -> SExpr -> Eval SExpr
lt a b = pure $ SBool $ a < b

callcc :: SExpr -> Eval SExpr
callcc op = callCC $ \k ->
  let cc = wrap $ SOp $ Op $ unary k
   in apply op $ SPair cc SNil

importOp :: SExpr -> Eval SExpr
importOp (SSym filename) = do
  content <- liftIO $ TIO.readFile (T.unpack filename)
  case parse single (T.unpack filename) content of
    Left err -> throwError $ RuntimeError (T.pack $ errorBundlePretty err)
    Right ast -> eval ast
importOp arg = throwError $ TypeError $ "import expects a filename, got " <> renderVal True arg

begin :: SExpr -> Eval SExpr
begin SNil = pure SNil
begin (SPair x SNil) = eval x
begin (SPair x rest) = eval x >> begin rest
begin _ = throwError $ TypeError "improper list"

primitives :: [(Text, SExpr)]
primitives =
  [ ("$define!", SOp $ Op $ binary $ \sym val -> define sym =<< eval val),
    ("$set!", SOp $ Op $ binary $ \sym val -> set sym =<< eval val),
    ("$if", SOp $ Op $ ternary if'),
    ("$vau", SOp $ Op $ ternary vau),
    ("wrap", wrap $ SOp $ Op $ unary $ pure . wrap),
    ("+", wrap $ SOp $ Op $ binary plus),
    ("neg", wrap $ SOp $ Op $ unary neg),
    ("*", wrap $ SOp $ Op $ binary times),
    ("/", wrap $ SOp $ Op $ binary divide),
    ("cons", wrap $ SOp $ Op $ binary cons),
    ("display!", wrap $ SOp $ Op $ unary display),
    ("show", wrap $ SOp $ Op $ unary show'),
    ("eq?", wrap $ SOp $ Op $ binary eq),
    ("<", wrap $ SOp $ Op $ binary lt),
    ("call/cc", wrap $ SOp $ Op $ unary callcc),
    ("$raw-import!", SOp $ Op $ unary importOp),
    ("$begin", SOp $ Op $ begin)
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
