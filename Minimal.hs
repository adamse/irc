{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# language ForeignFunctionInterface #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Minimal () where

import Foreign.C (newCString, CULong, CString, CInt(..))
import Foreign.StablePtr (newStablePtr, freeStablePtr, deRefStablePtr, StablePtr)
import Data.IORef (readIORef, IORef, newIORef)
import Control.Concurrent.Chan (readChan, writeChan, Chan, newChan)
import Control.Concurrent.Async (Async, async)
import Foreign.C.Types (CChar)
import Foreign (plusPtr, castPtr, Ptr)
import Foreign.Storable (Storable(..))
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (forever)

newtype TargetType = TargetType CInt

pattern TTUser :: TargetType
pattern TTUser = TargetType 0
pattern TTChannel :: TargetType
pattern TTChannel = TargetType 1
pattern TTServer :: TargetType
pattern TTServer = TargetType 2

instance Show TargetType where
  show = \case
    TTUser -> "TTUser"
    TTChannel -> "TTChannel"
    TTServer -> "TTServer"

data Message = Message
  { target :: Text
  , targetType :: TargetType
  , sender :: Text
  , message :: Text
  }
  deriving (Show)

data State = State
  { known_channels :: IORef (Set Text)
  , ircd_message_queue :: Chan Message
  , ircd_message_worker :: Async ()
  }

foreign export ccall hs_module_init :: IO (StablePtr State)
foreign export ccall hs_module_cleanup :: StablePtr State -> IO ()
foreign export ccall hs_module_version :: IO CString

hs_module_init :: IO (StablePtr State)
hs_module_init = do
  known_channels <- newIORef ["chan"]
  ircd_message_queue <- newChan

  ircd_message_worker <- async $ forever do
    msg <- readChan ircd_message_queue
    appendFile "m_minimal.log" (show msg <> "\n")

  newStablePtr State{..}

hs_module_cleanup :: StablePtr State -> IO ()
hs_module_cleanup state = do
  freeStablePtr state
  pure ()

hs_module_version :: IO CString
hs_module_version = newCString "This is a inspircd module implemented in GHC Haskell"

foreign export ccall hs_module_OnUserPostMessage :: StablePtr State -> Ptr Text -> TargetType -> Ptr Text -> Ptr Text -> IO CInt

hs_module_OnUserPostMessage :: StablePtr State -> Ptr Text -> TargetType -> Ptr Text -> Ptr Text -> IO CInt
hs_module_OnUserPostMessage state pnick targetType ptarget pmessage = do
  State{ircd_message_queue} <- deRefStablePtr state
  target <- peekString ptarget
  sender <- peekString pnick
  message <- peekString pmessage
  writeChan ircd_message_queue Message{..}
  pure 1

peekString :: Ptr Text -> IO Text
peekString ptr = do
  len :: CULong <- peek (castPtr ptr)
  data_ <- peek (ptr `plusPtr` sizeOf len)
  Text.peekCStringLen (data_, fromIntegral len)