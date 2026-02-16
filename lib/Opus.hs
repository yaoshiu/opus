{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Opus
  ( initialEnv,
    runLine,
  )
where

import Control.Monad (foldM)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Eval (eval, runEval)
import Parser (program, single)
import Prim (primEnv)
import SExpr (Env (..), EvalError (..), renderVal, displayError, SExpr(..))
import Text.Megaparsec (parse)

prelude :: Text
prelude = T.pack $(makeRelativeToProject "data/prelude.op" >>= embedStringFile)

runSource :: Env -> String -> Text -> IO (Maybe EvalError)
runSource env filename source = do
  case parse program filename source of
    Left _ -> pure Nothing
    Right asts -> do
      result <- runEval env (foldM (\_ expr -> eval expr) SNil asts)
      case result of
        Left err -> pure $ Just err
        Right _ -> pure Nothing

initialEnv :: IO Env
initialEnv = do
  prims <- primEnv
  preludeFrame <- newIORef Map.empty
  let stdlibEnv = Env (Just prims) preludeFrame
  _ <- runSource stdlibEnv "<prelude>" prelude
  globalFrame <- newIORef Map.empty
  pure $ Env (Just stdlibEnv) globalFrame

runLine :: Env -> Text -> IO Text
runLine env input = do
  case parse single "<opus>" input of
    Left err -> pure $ T.pack $ show err
    Right ast -> do
      result <- runEval env (eval ast)
      case result of
        Left err -> pure $ displayError err
        Right val -> pure $ renderVal True val
