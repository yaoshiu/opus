{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import System.IO (hFlush, stdout)
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Error (errorBundlePretty)
import Eval (eval, showVal, EnvCtx, runEval)
import qualified Data.Map as Map

repl :: EnvCtx -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- TIO.getLine
  unless (T.null line) $ do
    case parse (sc *> expr <* eof) "<repl>" line of
      Left err -> putStrLn (errorBundlePretty err)
      Right ast -> do
        v <- runEval env (eval ast)
        TIO.putStrLn $ showVal v
        repl env

main :: IO ()
main = repl $ Map.fromList []
