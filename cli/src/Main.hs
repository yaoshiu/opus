{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (readIORef)
import Data.List (isPrefixOf, nub)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Opus (OpusResult (..), initialEnv, runLine)
import SExpr (Env (..), renderVal)
import System.Console.Haskeline (CompletionFunc, completeWord, defaultSettings, getInputLine, runInputT, setComplete, simpleCompletion)
import System.Environment (getArgs)
import System.Exit (exitFailure)

allSymbols :: Env -> IO [String]
allSymbols Env {parent, frame} = do
  bindings <- readIORef frame
  let currentKeys = map T.unpack $ Map.keys bindings
  case parent of
    Nothing -> pure currentKeys
    Just p -> (currentKeys ++) <$> allSymbols p

completionFunc :: Env -> CompletionFunc IO
completionFunc env = completeWord Nothing " \t()\"[]" $ \str -> do
  symbols <- allSymbols env
  let matches = filter (str `isPrefixOf`) (nub symbols)
  return $ map simpleCompletion matches

repl :: Env -> IO ()
repl env = runInputT settings (loop "")
  where
    settings = setComplete (completionFunc env) defaultSettings
    loop acc = do
      let prompt = if null acc then "opus> " else "    | "
      minput <- getInputLine prompt
      case minput of
        Nothing -> pure ()
        Just "" -> loop ""
        Just input -> do
          let current = acc <> "\n" <> input
          res <- liftIO $ runLine env "<repl>" (T.pack current)
          case res of
            Incomplete -> loop current
            Success val -> do
              liftIO $ TIO.putStrLn $ renderVal True val
              loop ""
            Error err -> do
              liftIO $ TIO.putStrLn err
              loop ""

main :: IO ()
main = do
  args <- getArgs
  env <- initialEnv
  case args of
    [] -> do
      putStrLn "Welcome to Opus!"
      repl env
    [filename] -> do
      content <- TIO.readFile filename
      output <- runLine env filename content
      case output of
        Incomplete -> do
          TIO.putStrLn "unexpected error"
          exitFailure
        Error err -> do
          TIO.putStrLn err
          exitFailure
        Success {} -> pure ()
    _ -> do
      putStrLn "Usage: opus [filename]"
      exitFailure
