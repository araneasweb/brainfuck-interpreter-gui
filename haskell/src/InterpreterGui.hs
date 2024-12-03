{-# LANGUAGE OverloadedStrings, OverloadedLabels, InstanceSigs, FlexibleInstances #-}
module InterpreterGui(GUIState (..), run) where

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Control.Monad.Reader (MonadIO(liftIO), MonadReader(ask), ReaderT)
import Data.Binary (Word8)
import Tape (Tape(..), store, index)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified GI.GLib as GLib
import InterpreterBase (Interpreter(..), run)
import qualified GI.GObject.Functions as Gi.GObjects
import qualified Data.GI.Base as Gi.Gdk
import Data.Sequence (Seq (..))

import InterpreterBase (Interpreter(..), run, InterpreterState(..))
import Data.Binary (Word8)
import Data.Map (Map, lookup, insert, empty)
import Tape (store, index, Tape)
-- wrapper on InterpreterBase such that it works with gi-gtk :)

data GUIState = GUIState
  { outputView  :: Gtk.TextView
  , inputField  :: Gtk.Entry
  , inputToggle :: Gtk.Button
  , stepToggle  :: Gtk.Button
  , evalQueue   :: Seq InterpreterState
  }

type GUIMonad = ReaderT GUIState IO

instance Interpreter GUIMonad where
  recurseStandby :: (Int -> Map Int Int -> String -> (Tape Word8) -> GUIMonad (Tape Word8)) -> InterpreterState -> GUIMonad (Tape Word8) 
  recurseStandby f InterpreterState {readingIndex = readingIndex, bracketMap = bracketMap, sourceCode = sourceCode, tape = tape} = f readingIndex bracketMap sourceCode tape

  writeInputFromTape :: Tape Word8 -> GUIMonad (Tape Word8)
  writeInputFromTape tape = do
    GUIState {outputView = outputView} <- ask
    liftIO $ do
      resultVar <- newEmptyMVar
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        buffer <- #getBuffer outputView
        iter <- #getEndIter buffer
        Gtk.textBufferInsert buffer iter (T.pack [(toEnum.fromIntegral.index) tape]) (-1)
        putMVar resultVar ()
        return GLib.SOURCE_REMOVE
      takeMVar resultVar
      return tape

  writeInputToTape :: Tape Word8 -> GUIMonad (Tape Word8)
  writeInputToTape tape = do
      GUIState {inputField = inputField, inputToggle = inputToggle} <- ask
      resultVar <- liftIO newEmptyMVar
      handlerIdVar <- liftIO newEmptyMVar
      liftIO $ do
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
              #setSensitive inputField True
              #setSensitive inputToggle True
              handlerId <- Gtk.on inputToggle #clicked $ do
                  inputText <- Gtk.entryGetText inputField
                  Gtk.entrySetText inputField ""
                  #setSensitive inputField False
                  #setSensitive inputToggle False
                  putMVar resultVar inputText
              putMVar handlerIdVar handlerId
              return GLib.SOURCE_REMOVE
      inputText <- liftIO $ takeMVar resultVar
      handlerId <- liftIO $ takeMVar handlerIdVar
      liftIO $ Gi.GObjects.signalHandlerDisconnect inputToggle handlerId
      return (if T.null inputText then store 0 tape else store (fromIntegral.fromEnum $ T.head inputText) tape)
