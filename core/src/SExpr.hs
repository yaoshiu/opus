{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SExpr
  ( SExpr (..),
    Env (..),
    Eval (..),
    Op (..),
    Cell,
    unicodeSize,
    EvalError (..),
    EvalResult,
    renderVal,
    displayError,
  )
where

import Control.Monad.Cont (ContT, MonadCont)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.IORef (IORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

unicodeSize :: Int
unicodeSize = 1114112

data SExpr
  = SNil
  | SSym Text
  | SBool Bool
  | SNum Integer
  | SPair SExpr SExpr
  | SOp Op
  | SChar Char
  deriving (Eq, Ord)

newtype Op = Op (SExpr -> Eval SExpr)

instance Eq Op where
  _ == _ = False

instance Ord Op where
  _ <= _ = False

type Cell = IORef SExpr

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, frame :: Frame}

instance Eq Env where
  _ == _ = False

instance Ord Env where
  _ <= _ = False

data EvalError
  = TypeError Text
  | ArityError Int Int
  | RuntimeError Text

type EvalResult = Either EvalError SExpr

newtype Eval a = Eval
  { unEval ::
      ReaderT Env (ExceptT EvalError (ContT EvalResult IO)) a
  }
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadCont,
      MonadError EvalError
    )

asString :: SExpr -> Maybe String
asString SNil = Just ""
asString (SPair (SChar c) rest) =
  case asString rest of
    Just s -> Just (c : s)
    Nothing -> Nothing
asString _ = Nothing

renderVal :: Bool -> SExpr -> Text
renderVal _ (SNum n) = T.show n
renderVal _ SNil = "()"
renderVal _ (SBool True) = "true"
renderVal _ (SBool False) = "false"
renderVal render (SSym s) = (if render then "'" else "") <> s
renderVal _ (SOp {}) = "#<operative>"
renderVal render lst@(SPair l r) =
  case asString lst of
    Just s -> if render then T.show s else T.pack s
    Nothing ->
      let showTail SNil = ""
          showTail (SPair left right) =
            " " <> renderVal render left <> showTail right
          showTail right = " . " <> renderVal render right
       in "(" <> renderVal render l <> showTail r <> ")"
renderVal True (SChar c) = case c of
  ' ' -> "#\\w"
  '\n' -> "#\\n"
  '\t' -> "#\\t"
  _ -> T.pack $ '#' : [c]
renderVal False (SChar c) = T.pack [c]

displayError :: EvalError -> Text
displayError (TypeError msg) = "TypeError: " <> msg
displayError (ArityError l r) =
  "ArityError: expected "
    <> T.show l
    <> " got "
    <> T.show r
displayError (RuntimeError msg) = "RuntimeError: " <> msg
