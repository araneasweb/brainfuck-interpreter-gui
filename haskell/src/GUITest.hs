{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUITest where

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

activate :: Gtk.Application -> IO ()
activate app = do
  -- Initialising the run button with a shape constraint so it doesn't grow with resising
  button <- new Gtk.Button [#label := "Run"]
  lockedButton <- new Gtk.AspectFrame []
  set lockedButton [#ratio := 2.0, #obeyChild := True, #shadowType := Gtk.ShadowTypeNone]
  #add lockedButton button

  -- ditto but with the reset button
  resetButton <- new Gtk.Button [#sensitive := False, #label := "Reset"]
  lockedResetButton <- new Gtk.AspectFrame []
  set lockedResetButton [#ratio := 2.0, #obeyChild := True, #shadowType := Gtk.ShadowTypeNone]
  #add lockedResetButton resetButton

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

  -- on run click, the code should pull the info from the text extry and paste it into the tty and the output view, then lock itself out
  -- TODO hook into the brainfuck interpreter for interpretation instead of copy paste
  on button #clicked $ do
    set button [#sensitive := False]
    set resetButton [#sensitive := True]
    buffer <- #getBuffer entry
    startIter <- #getStartIter buffer
    endIter <- #getEndIter buffer
    text <- #getText buffer startIter endIter False
    putStrLn $ T.unpack text
    outputBuffer <- #getBuffer outputView
    Gtk.textBufferSetText outputBuffer text (-1)

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
  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  buttonBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #packStart hbox scrolledWindowEntry True True 32
  #packStart vbox scrolledWindowOutput True True 32
  #packStart buttonBox lockedButton False False 16
  #packStart buttonBox lockedResetButton False False 16
  #packStart vbox buttonBox True True 32
  #packStart hbox vbox True True 32

  -- adding bounding box to main window
  window <- new Gtk.ApplicationWindow [#application := app, #title := "Brainfuck Integrated Development Environment"]
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
