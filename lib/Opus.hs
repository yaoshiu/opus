{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Opus
  ( initialEnv,
    runLine,
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Eval (eval, runEval)
import Parser (single)
import Prim (primEnv)
import SExpr (Env (..), renderVal, displayError)
import Text.Megaparsec (parse, errorBundlePretty)

prelude :: Text
prelude = T.pack $(makeRelativeToProject "data/prelude.op" >>= embedStringFile)

initialEnv :: IO Env
initialEnv = do
  prims <- primEnv
  preludeFrame <- newIORef Map.empty
  let stdlibEnv = Env (Just prims) preludeFrame
  _ <- runLine stdlibEnv "<prelude>" prelude
  globalFrame <- newIORef Map.empty
  pure $ Env (Just stdlibEnv) globalFrame

runLine :: Env -> String -> Text -> IO Text
runLine env sourceName input = do
  case parse single sourceName input of
    Left err -> pure $ T.pack $ errorBundlePretty err
    Right ast -> do
      result <- runEval env (eval ast)
      case result of
        Left err -> pure $ displayError err
        Right val -> pure $ renderVal True val
