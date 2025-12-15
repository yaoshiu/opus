{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT), ask)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Parser (SExpr (..))
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import qualified Data.Text.IO as TIO

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VFunc [Text] SExpr EnvCtx
  | VPair Value Value
  | VNil

data EvalError
  = UnboundVariable Text

newtype Eval a = Eval {unEval :: ReaderT EnvCtx (ExceptT EvalError IO) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO,
      MonadError EvalError
    )

type EnvCtx = Map.Map Text Value

printError :: EvalError -> IO ()
printError (UnboundVariable x) = TIO.putStrLn ("unbound variable: " <> x)

runEval :: EnvCtx -> Eval a -> IO (Either EvalError a)
runEval env ev = runExceptT (runReaderT (unEval ev) env)

showVal :: Value -> Text
showVal (VNumber n) = T.pack $ show n
showVal (VString s) = "\"" <> s <> "\""
showVal VNil = "()"
showVal (VBoolean True) = "#t"
showVal (VBoolean False) = "#f"
showVal (VSymbol s) = s
showVal (VPrim _) = "#<primitive>"
showVal (VFunc _ _ _) = "#<procedure>"
showVal (VPair l r) =
  let showTail VNil = ""
      showTail (VPair l r) = " " <> showVal l <> showTail r
      showTail r = " . " <> showVal r
   in "(" <> showVal l <> showTail r <> ")"

datumToValue :: SExpr -> Value
datumToValue (PNumber n) = VNumber n
datumToValue (PBoolean b) = VBoolean b
datumToValue (PString s) = VString s
datumToValue (PSymbol s) = VSymbol s
datumToValue (PList xs) = foldr VPair VNil (map datumToValue xs)
datumToValue (PDotted xs t) =
  foldr VPair (datumToValue t) (map datumToValue xs)

eval :: SExpr -> Eval Value
eval (PList []) = pure VNil
eval (PList [PSymbol "quote", x]) = pure $ datumToValue x
eval (PSymbol s) = getVar s
eval (PList [PSymbol "let", PList pairs, expr]) = 

getVar :: Text -> Eval Value
getVar s = do
  env <- ask
  case Map.lookup s env of
    Just x -> pure x
    Nothing -> throwError (UnboundVariable s)

