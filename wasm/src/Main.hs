{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import qualified Data.Text as T
import Foreign.StablePtr
  ( StablePtr,
    deRefStablePtr,
    freeStablePtr,
    newStablePtr,
  )
import Opus (OpusResult (..), initialEnv, runLine)
import SExpr (Env, SExpr, renderVal)

#ifdef wasm32_HOST_ARCH
import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString, JSVal)

type JSFunction a = JSVal

foreign export javascript "init_env" initEnv :: IO (StablePtr Env)

foreign export javascript "eval"
  eval ::
    EnvPtr ->
    JSString ->
    JSString ->
    JSFunction (SExprPtr -> IO ()) ->
    JSFunction (IO ()) ->
    JSFunction (JSString -> IO ()) ->
    IO ()

foreign export javascript "render"
  render :: StablePtr SExpr -> Bool -> IO JSString

foreign export javascript "hs_free"
  hsFree :: StablePtr a -> IO ()

foreign import javascript "dynamic"
  runSuccess ::
    JSFunction (StablePtr SExpr -> IO ()) ->
    StablePtr SExpr ->
    IO ()

foreign import javascript "dynamic"
  runIncomp :: JSFunction (IO ()) -> IO ()

foreign import javascript "dynamic"
  runErr :: JSFunction (JSString -> IO ()) -> JSString -> IO ()
#else
newtype JSString = JSString { fromJSString :: String }
newtype JSFunction a = JSFunction { runJS :: a }

toJSString :: String -> JSString
toJSString = JSString

runSuccess :: JSFunction (StablePtr SExpr -> IO ()) -> StablePtr SExpr -> IO ()
runSuccess = runJS

runIncomp :: JSFunction (IO ()) -> IO ()
runIncomp = runJS

runErr :: JSFunction (JSString -> IO ()) -> JSString -> IO ()
runErr = runJS
#endif

type EnvPtr = StablePtr Env

type SExprPtr = StablePtr SExpr

initEnv :: IO (StablePtr Env)
initEnv = initialEnv >>= newStablePtr

eval ::
  EnvPtr ->
  JSString ->
  JSString ->
  JSFunction (SExprPtr -> IO ()) ->
  JSFunction (IO ()) ->
  JSFunction (JSString -> IO ()) ->
  IO ()
eval envPtr input source success incomp err = do
  let code = T.pack $ fromJSString input
  let src = fromJSString source
  env <- deRefStablePtr envPtr
  res <- runLine env src code
  case res of
    Success val -> newStablePtr val >>= runSuccess success
    Incomplete -> runIncomp incomp
    Error msg -> runErr err $ toJSString $ T.unpack msg

render :: StablePtr SExpr -> Bool -> IO JSString
render ptr quote = do
  val <- deRefStablePtr ptr
  pure $ toJSString $ T.unpack $ renderVal quote val

hsFree :: StablePtr a -> IO ()
hsFree = freeStablePtr

main :: IO ()
main = pure ()
