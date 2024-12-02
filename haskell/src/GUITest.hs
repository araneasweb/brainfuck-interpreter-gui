{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUITest where

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import Data.GI.Base (AttrOp((:=)), on, set, new)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO(liftIO), ReaderT(runReaderT))
import InterpreterGui (run, GUIState (..))
import Data.Binary (Word8)
import Tape (Tape(..), store, index)
import Control.Concurrent (forkIO)
import qualified GI.GLib as GLib

activate :: Gtk.Application -> IO ()
activate app = do
  -- Initialising the run button with a shape constraint
  button <- new Gtk.Button [ #label := "Run" ]
  lockedButton <- new Gtk.AspectFrame []
  set lockedButton [ #ratio      := 2.0
                   , #obeyChild  := True
                   , #shadowType := Gtk.ShadowTypeNone ]
  #add lockedButton button

  -- ditto but with the reset button
  resetButton <- new Gtk.Button [ #sensitive := False
                                , #label     := "Reset" ]
  lockedResetButton <- new Gtk.AspectFrame []
  set lockedResetButton [ #ratio      := 2.0
                        , #obeyChild  := True
                        , #shadowType := Gtk.ShadowTypeNone]
  #add lockedResetButton resetButton

  -- bottom half of the console that works with the "," command
  sendButton <- new Gtk.Button [ #sensitive := False
                               , #label := "Send" ]
  lockedSendButton <- new Gtk.AspectFrame []
  set lockedSendButton [ #ratio      := 2.0
                       , #obeyChild  := True
                       , #shadowType := Gtk.ShadowTypeNone]
  #add lockedSendButton sendButton
  inputEntry <- new Gtk.Entry [ #sensitive := False ]
  inputBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  #packStart inputBox inputEntry True True 0
  #packStart inputBox lockedSendButton False False 0


  -- adding the brainfuck text entry box and the pseudo-cout box
  entry <- new Gtk.TextView []
  outputView <- new Gtk.TextView []
  Gtk.textViewSetEditable outputView False
  scrolledWindowEntry <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindowEntry Gtk.PolicyTypeAlways Gtk.PolicyTypeAutomatic
  #add scrolledWindowEntry entry
  scrolledWindowOutput <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindowOutput Gtk.PolicyTypeAlways Gtk.PolicyTypeAutomatic
  #add scrolledWindowOutput outputView
  #setSizeRequest scrolledWindowEntry 256 128
  #setSizeRequest scrolledWindowOutput 256 128

  -- starting interpreter when run button is clicked
  on button #clicked $ do
    set button [#sensitive := False]
    set resetButton [#sensitive := True]
    buffer <- #getBuffer entry
    startIter <- #getStartIter buffer
    endIter <- #getEndIter buffer
    text <- #getText buffer startIter endIter False
    outputBuffer <- #getBuffer outputView
    Gtk.textBufferSetText outputBuffer "" (-1)
    _ <- liftIO $ forkIO $ void $
      runReaderT (InterpreterGui.run (T.unpack text))
        GUIState { outputView  = outputView
                 , inputField  = inputEntry
                 , inputToggle = sendButton }
    return ()

  -- reset clears the output view, unlocks run, and locks itself
  on resetButton #clicked $ do
    set button [#sensitive := True]
    set resetButton [#sensitive := False]
    outputBuffer <- #getBuffer outputView
    Gtk.textBufferSetText outputBuffer "" (-1)

  -- GUI margins
  #setMarginTop scrolledWindowEntry 32
  #setMarginBottom scrolledWindowEntry 32
  #setMarginTop button 32
  #setMarginBottom button 32
  #setMarginTop resetButton 32
  #setMarginBottom resetButton 32

  -- adding all the elements to boxes for shape and bounding
  hbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  buttonBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  terminal <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #packStart hbox scrolledWindowEntry True True 32
  #packStart terminal scrolledWindowOutput True True 0
  #packStart terminal inputBox False False 0
  #setSizeRequest inputBox (-1) 32
  #packStart vbox terminal True True 32
  #packStart buttonBox lockedButton False False 16
  #packStart buttonBox lockedResetButton False False 16
  #packStart vbox buttonBox True True 32
  #packStart hbox vbox True True 32

  

  -- adding bounding box to main window
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Brainfuck Integrated Development Environment" ]
  #add window hbox

  #showAll window

run :: IO ()
run = do
  maybeApp <- Gtk.applicationNew (Just "foss.brainfuck-ide") []

  case maybeApp of
    Nothing -> putStrLn "Failed to create the GTK application."
    Just app -> do
      on app #activate (activate app)
      void $ #run app Nothing

main :: IO ()
main = GUITest.run
