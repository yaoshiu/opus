{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( Value (..),
    IFunc (..),
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
  | VProc IFunc
  | VPair Value Value
  | VNil
  | VMacro Text SExpr Env
  deriving (Eq, Ord)

newtype IFunc = IFunc {fn :: [Value] -> Eval Value}

instance Eq IFunc where
  _ == _ = False

instance Ord IFunc where
  _ <= _ = False

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
  let showPair expr =
        case expr of
          PPair l PNil -> showSExpr l
          PPair l r@(PPair {}) -> showSExpr l <> showPair r
          PPair l r -> showSExpr l <> " . " <> showSExpr r
          _ -> error "improper list"
   in "(" <> showPair pair <> ")"
showSExpr (PNumber n) = T.show n
showSExpr (PString s) = "\"" <> s <> "\""
showSExpr (PSymbol s) = s
showSExpr PNil = "()"

renderVal :: Bool -> Value -> Text
renderVal _ (VNumber n) = T.pack $ show n
renderVal True (VString s) = "\"" <> s <> "\""
renderVal False (VString s) = s
renderVal _ VNil = "()"
renderVal _ (VBoolean True) = "#t"
renderVal _ (VBoolean False) = "#f"
renderVal _ (VSymbol s) = s
renderVal _ (VProc {}) = "#<procedure>"
renderVal r (VPair l r') =
  let showTail VNil = ""
      showTail (VPair left right) = " " <> renderVal r left <> showTail right
      showTail right = " . " <> renderVal r right
   in "(" <> renderVal r l <> showTail r' <> ")"
renderVal _ (VMacro {}) = "#<macro>"

showVal :: Value -> Text
showVal = renderVal True

datumToValue :: SExpr -> Value
datumToValue (PNumber n) = VNumber n
datumToValue (PBoolean b) = VBoolean b
datumToValue (PString s) = VString s
datumToValue (PSymbol s) = VSymbol s
datumToValue (PPair l r) = VPair (datumToValue l) (datumToValue r)
datumToValue PNil = VNil

valueToDatum :: Value -> Eval SExpr
valueToDatum (VNumber n) = pure $ PNumber n
valueToDatum (VBoolean b) = pure $ PBoolean b
valueToDatum (VString s) = pure $ PString s
valueToDatum (VSymbol s) = pure $ PSymbol s
valueToDatum VNil = pure $ PNil
valueToDatum (VPair l r) = PPair <$> (valueToDatum l) <*> (valueToDatum r)
valueToDatum (VProc {}) = throwError $ SyntaxError "macros cannot return procedures"
valueToDatum (VMacro {}) = throwError $ SyntaxError "macros cannot return macros"

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

evalDo :: SExpr -> Eval Value
evalDo (PNil) = pure $ VNil
evalDo (PPair l PNil) = eval l
evalDo (PPair l r) = eval l >> evalDo r
evalDo _ = throwError $ SyntaxError "cannot evaluate an improper list"

evalDefine :: Text -> Value -> Eval Value
evalDefine name val = do
  Env {bindings} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' bindings $ Map.insert name cell
  pure val

evalLambda :: SExpr -> SExpr -> Eval Value
evalLambda params expr = do
  params' <- getParams params
  env <- ask
  let func args
        | length params' /= length args =
            throwError $ ArityError (length params') (length args)
        | otherwise = do
            args' <- mapM (liftIO . newIORef) args
            bindings <- liftIO $ newIORef $ Map.fromList $ zip params' args'
            let env' = Env (Just env) bindings
            local (const env') $ eval expr
  pure (VProc $ IFunc func)

getParams :: SExpr -> Eval [Text]
getParams PNil = pure $ []
getParams (PPair (PSymbol l) r) = (l :) <$> getParams r
getParams bad = throwError $ SyntaxError $ "invalid parameter: " <> showSExpr bad

apply :: Value -> [Value] -> Eval Value
apply (VProc f) args = fn f args
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

eval :: SExpr -> Eval Value
eval (PSymbol s) = getVar s >>= liftIO . readIORef
eval (PPair (PSymbol "if") (PPair pre (PPair con (PPair alt PNil)))) =
  evalIf pre con alt
eval (PPair (PSymbol "quote") (PPair x PNil)) = pure $ datumToValue x
eval (PPair (PSymbol "lambda") (PPair params@(PPair {}) (PPair body PNil))) = do
  evalLambda params body
eval (PPair (PSymbol "do") body) = evalDo body
eval (PPair (PSymbol "define!") (PPair (PSymbol name) (PPair body PNil))) =
  eval body >>= evalDefine name
eval (PPair (PSymbol "define!") (PPair (PPair (PSymbol name) params) (PPair body PNil))) =
  evalLambda params body >>= evalDefine name
eval (PPair (PSymbol "set!") (PPair (PSymbol name) (PPair body PNil))) =
  eval body >>= evalSet name
eval (PPair (PSymbol "macro") (PPair (PSymbol param) (PPair body PNil))) =
  evalMacro param body
eval (PPair fExpr args) = do
  fVal <- eval fExpr
  case fVal of
    VMacro {} -> do
      let args' = datumToValue args
      expanded <- applyMacro fVal args'
      datum <- valueToDatum expanded
      eval datum
    _ -> do
      values <- evalList args
      apply fVal values
eval x = pure $ datumToValue x
