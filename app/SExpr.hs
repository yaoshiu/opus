{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SExpr
  ( SExpr (..),
    IFunc (..),
    EvalError (..),
    Env (..),
    Eval (..),
    Cell,
  )
where

import Control.Monad.Cont (ContT, MonadCont)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.IORef (IORef)
import qualified Data.Map as Map
import Data.Text (Text)

data SExpr
  = SNumber Integer
  | SSymbol Text
  | SString Text
  | SBoolean Bool
  | SPair SExpr SExpr
  | SNil
  | SProc IFunc
  | SMacro IFunc
  deriving (Show, Eq, Ord)

newtype IFunc = IFunc {fn :: SExpr -> Eval SExpr}

instance Eq IFunc where
  _ == _ = False

instance Ord IFunc where
  _ <= _ = False

instance Show IFunc where
  show _ = error "cannot show a function"

data EvalError
  = UnboundVariable Text
  | TypeError Text
  | ArityError Int Int
  | SyntaxError Text
  | NumericError Text

type EvalResult = Either EvalError SExpr

type Cell = IORef SExpr

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, bindings :: Frame}

instance Eq Env where
  _ == _ = False

instance Ord Env where
  _ <= _ = False

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
      MonadError EvalError,
      MonadCont
    )
