{-# language ForeignFunctionInterface #-}
{-# language OverloadedStrings #-}
module Minimal () where

import Prelude hiding (init)
import Foreign.C (newCString, CString, CInt(..))
import Foreign.StablePtr (newStablePtr, freeStablePtr, StablePtr)

data State = State

foreign export ccall hs_module_init :: IO (StablePtr State)
foreign export ccall hs_module_cleanup :: (StablePtr State) -> IO CInt
foreign export ccall hs_module_version :: IO CString

hs_module_init :: IO (StablePtr State)
hs_module_init = do
  putStrLn "Hello"
  newStablePtr State

hs_module_cleanup :: StablePtr State -> IO CInt
hs_module_cleanup state = do 
  putStrLn "Cleanup!" 
  freeStablePtr state 
  pure 1

hs_module_version :: IO CString
hs_module_version = newCString "This is a inspircd module implemented in GHC Haskell"