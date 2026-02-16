{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Opus (initialEnv, runLine)
import SExpr (Env)
import qualified Data.Text as T

foreign export ccall "opus_malloc" mallocBytes :: Int -> IO (Ptr a)
foreign export ccall "opus_free" free :: Ptr a -> IO ()

foreign export ccall "opus_new_env" opusNewEnv :: IO (StablePtr Env)

opusNewEnv :: IO (StablePtr Env)
opusNewEnv = initialEnv >>= newStablePtr

foreign export ccall "opus_free_env" freeStablePtr :: StablePtr Env -> IO ()

foreign export ccall "opus_eval" opusEval :: StablePtr Env -> CString -> IO CString

opusEval :: StablePtr Env -> CString -> IO CString
opusEval envPtr inputPtr = do
    env <- deRefStablePtr envPtr
    inputStr <- peekCString inputPtr
    outputTxt <- runLine env (T.pack inputStr)
    newCString (T.unpack outputTxt)

main :: IO ()
main = pure ()
