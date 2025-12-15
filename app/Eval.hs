{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import qualified Data.Map as Map
import Data.Text (Text)
import Parser (Expr (..))
import qualified Data.Text as T

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VFunc [Text] Expr EnvCtx
  | VPair Value Value
  | VNil

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

type EnvCtx = Map.Map Text Value

runEval :: EnvCtx -> Eval a -> IO a
runEval env ev = runReaderT (unEval ev) env

showVal :: Value -> Text
showVal (VNumber n) = T.pack $ show n
showVal (VString s) = "\"" <> s <> "\""
showVal VNil = "()"
showVal (VPair l r) = let
    showTail VNil = ""
    showTail (VPair l r) = " " <> showVal l <> showTail r
    showTail r = " . " <> showVal r
  in"(" <> showVal l <> showTail r <> ")"
showVal (VBoolean True) = "#t"
showVal (VBoolean False) = "#f"
showVal (VSymbol s) = s
showVal (VPrim _) = "#<primitive>"
showVal (VFunc _ _ _) = "#<procedure>"

eval :: Expr -> Eval Value
eval (Number n) = pure $ VNumber n
eval (String s) = pure $ VString s
eval (Boolean b) = pure $ VBoolean b
eval (List []) = pure $ VNil
