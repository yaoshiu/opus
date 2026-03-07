{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Foldable (for_, traverse_)
import Data.IORef (readIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import Foreign.StablePtr
  ( StablePtr,
    deRefStablePtr,
    freeStablePtr,
    newStablePtr,
  )
import Opus (OpusResult (..), initialEnv, runLine)
import SExpr (Env (..), SExpr, renderVal)

#ifdef wasm32_HOST_ARCH
import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString, JSVal)

type JSFunction a = JSVal
type JSArray a = JSVal

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

foreign export javascript "symbols"
  symbols :: EnvPtr -> IO (JSArray JSString)

foreign import javascript unsafe "[]" create_js_array :: IO (JSArray a)

foreign import javascript unsafe "$1.push($2)"
  js_push_str :: JSArray JSString -> JSString -> IO ()

foreign import javascript unsafe "dynamic"
  runSuccess ::
    JSFunction (StablePtr SExpr -> IO ()) ->
    StablePtr SExpr ->
    IO ()

foreign import javascript unsafe "dynamic"
  runIncomp :: JSFunction (IO ()) -> IO ()

foreign import javascript unsafe "dynamic"
  runErr :: JSFunction (JSString -> IO ()) -> JSString -> IO ()
#else
import Data.IORef (IORef, newIORef, modifyIORef')

newtype JSString = JSString { fromJSString :: String }
newtype JSFunction a = JSFunction { runJS :: a }
type JSArray a = IORef ([a] -> [a])

create_js_array :: IO (JSArray a)
create_js_array = newIORef id

js_push_str :: JSArray JSString -> JSString -> IO ()
js_push_str arr str = modifyIORef' arr (. (str:))

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

pushSyms :: Env -> JSArray JSString -> IO ()
pushSyms Env {parent, frame} array = do
  bindings <- readIORef frame
  for_
    (Map.keys bindings)
    (\sym -> js_push_str array $ toJSString $ T.unpack sym)
  traverse_ (`pushSyms` array) parent

symbols :: EnvPtr -> IO (JSArray JSString)
symbols envPtr = do
  env <- deRefStablePtr envPtr
  array <- create_js_array
  env `pushSyms` array
  pure array


main :: IO ()
main = pure ()
