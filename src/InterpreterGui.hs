{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Module: InterpreterGui
module InterpreterGui(GUIState (..), run, EvalState(..)) where

import Control.Concurrent (MVar (..), newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Data.Binary (Word8)
import qualified Data.GI.Base as Gi.Gdk
import Data.IORef (IORef)
import Data.Map (Map, empty, insert, lookup)
import qualified Data.Text as T
import GHC.IORef (IORef, writeIORef)
import GI.Gdk (AttrOp ((:=)), set)
import qualified GI.GLib as GLib
import qualified GI.GObject.Functions as Gi.GObjects
import qualified GI.Gtk as Gtk
import InterpreterBase (Interpreter (..), InterpreterState (..), run)
import Tape (Tape (..), index, store)
import TapeGUI (drawTape)
-- wrapper on InterpreterBase such that it works with gi-gtk :)

data EvalState = RunMode | StepMode

data GUIState = GUIState
  { outputView  :: Gtk.TextView
  , inputField  :: Gtk.Entry
  , inputToggle :: Gtk.Button
  , evalState   :: EvalState
  , stepperLock :: MVar ()
  , tapeRef     :: IORef (Tape Word8)
  , tapeGrid    :: Gtk.Grid
  , nextButton  :: Gtk.Button
  , offsetRef   :: IORef Int
  }

type GUIMonad = ReaderT GUIState IO

instance Interpreter GUIMonad where
  recurseStandby :: (Int -> Map Int Int -> String -> Tape Word8 -> GUIMonad (Tape Word8)) -> InterpreterState -> GUIMonad (Tape Word8)
  recurseStandby f InterpreterState { readingIndex = readingIndex
                                    , bracketMap   = bracketMap
                                    , sourceCode   = sourceCode
                                    , tape         = tape } = do
    GUIState { stepperLock = stepperLock
             , evalState  = evalState
             , tapeRef    = tapeRef
             , tapeGrid   = tapeGrid
             , nextButton = nextButton
             , offsetRef  = offsetRef } <- ask
    liftIO $ writeIORef tapeRef tape
    liftIO $ void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
      writeIORef offsetRef 0
      drawTape tapeGrid tapeRef offsetRef
      return GLib.SOURCE_REMOVE
    if readingIndex >= length sourceCode
      then do
        liftIO $ void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
          set nextButton [#sensitive := False]
          return GLib.SOURCE_REMOVE
        return tape
      else case evalState of
        RunMode -> f readingIndex bracketMap sourceCode tape
        StepMode -> do
          liftIO $ takeMVar stepperLock
          f readingIndex bracketMap sourceCode tape

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
