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
import qualified Control.Concurrent.Async as Async
import Foreign.C.Types (CChar)
import Foreign (allocaBytes, FunPtr, plusPtr, castPtr, Ptr)
import Foreign.Storable (Storable(..))
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (forever)
import qualified Data.Text as Text

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


type Logger = LogLevel -> Text -> IO ()


data State = State
  { known_channels :: IORef (Set Text)
  , ircd_message_queue :: Chan Message
  , ircd_message_worker :: Async ()
  , ircd_logger :: Logger
  }

newtype LogLevel = LogLevel CInt

pattern LOG_RAWIO :: LogLevel
pattern LOG_RAWIO = LogLevel 5
pattern LOG_DEBUG :: LogLevel
pattern LOG_DEBUG = LogLevel 10
pattern LOG_VERBOSE :: LogLevel
pattern LOG_VERBOSE = LogLevel 20
pattern LOG_DEFAULT :: LogLevel
pattern LOG_DEFAULT = LogLevel 30
pattern LOG_SPARSE :: LogLevel
pattern LOG_SPARSE = LogLevel 40
pattern LOG_NONE :: LogLevel
pattern LOG_NONE = LogLevel 5


type CLogger = LogLevel -> Ptr Text -> IO ()

foreign import ccall "dynamic" mkLogger :: FunPtr CLogger -> CLogger


type Init = FunPtr CLogger -> IO (StablePtr State)

foreign export ccall hs_module_init :: Init

hs_module_init :: Init
hs_module_init logger' = do
  let logger = mkLogger logger'
  let ircd_logger lvl t = withCString t (logger lvl)

  known_channels <- newIORef ["chan"]
  ircd_message_queue <- newChan

  ircd_message_worker <- async $ forever do
    msg <- readChan ircd_message_queue
    let msg' = "recieved: " <> show msg <> "\n"
    ircd_logger LOG_DEFAULT (Text.pack msg')
    appendFile "m_minimal.log" msg'

  newStablePtr State{..}


type Cleanup = StablePtr State -> IO ()

foreign export ccall hs_module_cleanup :: Cleanup

hs_module_cleanup :: Cleanup
hs_module_cleanup state = do
  State{ircd_message_worker} <- deRefStablePtr state
  Async.cancel ircd_message_worker
  freeStablePtr state
  pure ()


type Version = IO CString

foreign export ccall hs_module_version :: Version

hs_module_version :: Version
hs_module_version = newCString "This is a inspircd module implemented in GHC Haskell" -- TODO: this leaks the string


type OnUserPostMessage = StablePtr State -> Ptr Text -> TargetType -> Ptr Text -> Ptr Text -> IO ()

foreign export ccall hs_module_OnUserPostMessage :: OnUserPostMessage

hs_module_OnUserPostMessage :: OnUserPostMessage
hs_module_OnUserPostMessage state pnick targetType ptarget pmessage = do
  State{ircd_message_queue} <- deRefStablePtr state
  target <- peekCString ptarget
  sender <- peekCString pnick
  message <- peekCString pmessage
  writeChan ircd_message_queue Message{..}


peekCString :: Ptr Text -> IO Text
peekCString ptr = do
  len :: CULong <- peek (castPtr ptr)
  data_ <- peek (ptr `plusPtr` sizeOf len)
  Text.peekCStringLen (data_, fromIntegral len)

withCString :: Text -> (Ptr Text -> IO a) -> IO a
withCString t act = Text.withCStringLen t \(data_, len') ->
  let
    len :: CULong = fromIntegral len'
    sz = sizeOf len + sizeOf data_
  in allocaBytes sz \ptr -> do
    poke ptr len
    poke (ptr `plusPtr` sizeOf len) data_
    act (castPtr ptr)