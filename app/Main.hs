{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (readIORef)
import Data.List (isPrefixOf, nub)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Opus (initialEnv, runLine)
import SExpr (Env (..))
import System.Console.Haskeline (CompletionFunc, completeWord, defaultSettings, getInputLine, runInputT, setComplete, simpleCompletion)
import System.Environment (getArgs)

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
repl env = runInputT settings loop
  where
    settings = setComplete (completionFunc env) defaultSettings
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> pure ()
        Just "" -> loop
        Just input -> do
          output <- liftIO $ runLine env "<repl>" (T.pack input)
          liftIO $ TIO.putStrLn output
          loop

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
      TIO.putStrLn output
    _ -> putStrLn "Usage: opus [filename]"
