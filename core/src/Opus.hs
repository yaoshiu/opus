{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Opus
  ( initialEnv,
    runLine,
    OpusResult (..),
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Eval (eval, runEval)
import Parser (single)
import Prim (primEnv)
import SExpr (Env (..), SExpr (..), displayError)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error
  ( ErrorItem (EndOfInput),
    ParseError (TrivialError),
    ParseErrorBundle (bundleErrors),
    errorBundlePretty,
  )

prelude :: Text
prelude = T.pack $(makeRelativeToProject "data/prelude.op" >>= embedStringFile)

data OpusResult
  = Success SExpr
  | Incomplete
  | Error Text

isEOFError :: ParseErrorBundle Text Void -> Bool
isEOFError bundle = any checkError (bundleErrors bundle)
  where
    checkError (TrivialError _ (Just EndOfInput) _) = True
    checkError _ = False

initialEnv :: IO Env
initialEnv = do
  prims <- primEnv
  preludeFrame <- newIORef Map.empty
  let stdlibEnv = Env (Just prims) preludeFrame
  res <- runLine stdlibEnv "<prelude>" prelude
  case res of
    Incomplete -> error "unexpected eof in <prelude>"
    Error err -> error $ T.unpack err
    Success _ -> do
      globalFrame <- newIORef Map.empty
      pure $ Env (Just stdlibEnv) globalFrame

runLine :: Env -> String -> Text -> IO OpusResult
runLine env sourceName input = do
  case parse single sourceName input of
    Left err ->
      if isEOFError err
        then pure $ Incomplete
        else pure $ Error $ T.pack $ errorBundlePretty err
    Right ast -> do
      result <- runEval env $ eval ast
      case result of
        Left err -> pure $ Error $ displayError err
        Right val -> pure $ Success val
