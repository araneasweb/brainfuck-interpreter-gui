
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |Module: GUITest
module GUITest where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Binary (Word8)
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef, IORef)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import InterpreterGui (EvalState (..), GUIState (..), run)
import Tape (Tape (..), index, shiftLeft, shiftRight, store)
import TapeGUI (drawTape)

activate :: Gtk.Application -> IO ()
activate app = do

  -- makes turing simulator box
  (simVbox, tapeGrid, tapeRef, offsetRef, initialTape) <- makeTuringSim

  -- run and reset buttons
  (button,      lockedButton     ) <- createButton "Run"   True
  (resetButton, lockedResetButton) <- createButton "Reset" False
  (stepButton,  lockedStepButton ) <- createButton "Step"  True
  (nextButton,  lockedNextButton ) <- createButton "Next"  False

  -- bottom half of the console that works with the "," command
  (sendButton, lockedSendButton)   <- createButton "Send" False
  inputEntry <- new Gtk.Entry [ #sensitive := False ]
  inputBox   <- new Gtk.Box   [ #orientation := Gtk.OrientationHorizontal ]
  #packStart inputBox inputEntry True True 0
  #packStart inputBox lockedSendButton False False 0

  -- adding the brainfuck text entry box and the pseudo-cout box
  entry      <- new Gtk.TextView []
  outputView <- new Gtk.TextView []
  Gtk.textViewSetEditable outputView False
  scrolledWindowEntry  <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindowEntry Gtk.PolicyTypeAlways Gtk.PolicyTypeAutomatic
  #add scrolledWindowEntry entry
  scrolledWindowOutput <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindowOutput Gtk.PolicyTypeAlways Gtk.PolicyTypeAutomatic
  #add scrolledWindowOutput outputView
  #setSizeRequest scrolledWindowEntry 256 128
  #setSizeRequest scrolledWindowOutput 256 128

  buffer   <- #getBuffer entry
  tagTable <- #getTagTable buffer

  commentTag <- createTag "#6c6f85" tagTable
  errorTag   <- createTag "#d20f39" tagTable

  bracketTags <- mapM (\colour -> Gtk.textTagNew Nothing >>=
    \tag -> #add tagTable tag >> set tag [#foreground := colour] >> pure tag)
                      ["#df8e1d", "#40a02b", "#04a5e5", "#7287fd"]

  on buffer #changed $ do
    startIter <- #getStartIter buffer
    endIter   <- #getEndIter buffer
    #removeAllTags buffer startIter endIter
    #applyTag buffer commentTag startIter endIter
    applyTag "#dc8a78" "+" buffer tagTable
    applyTag "#dc8a78" "-" buffer tagTable
    applyTag "#7287fd" ">" buffer tagTable
    applyTag "#7287fd" "<" buffer tagTable
    applyTag "#8839ef" "." buffer tagTable
    applyTag "#8839ef" "," buffer tagTable
    matchBrackets buffer errorTag bracketTags

  stepperLock <- newEmptyMVar
  interpreterThreadRef <- newIORef Nothing

  -- starting interpreter when run button is clicked
  on button #clicked $ do
    set button [#sensitive := False]
    set resetButton [#sensitive := True]
    buffer       <- #getBuffer entry
    startIter    <- #getStartIter buffer
    endIter      <- #getEndIter buffer
    text         <- #getText buffer startIter endIter False
    outputBuffer <- #getBuffer outputView
    Gtk.textBufferSetText outputBuffer "" (-1)
    threadId <- forkIO $ void $
      runReaderT (InterpreterGui.run (T.unpack text))
        GUIState { outputView , inputEntry
                 , inputToggle = sendButton
                 , evalState   = RunMode
                 , stepperLock , tapeRef
                 , tapeGrid, nextButton
                 , offsetRef }
    writeIORef interpreterThreadRef (Just threadId)
    pure ()

  -- reset clears the output view, unlocks run, and locks itself
  on resetButton #clicked $ do
    set button      [#sensitive := True]
    set stepButton  [#sensitive := True]
    set resetButton [#sensitive := False]
    set nextButton  [#sensitive := False]
    outputBuffer <- #getBuffer outputView
    tryTakeMVar stepperLock
    Gtk.textBufferSetText outputBuffer "" (-1)
    writeIORef tapeRef initialTape
    writeIORef offsetRef 0
    drawTape tapeGrid tapeRef offsetRef
    readIORef interpreterThreadRef >>= maybe
      (pure ())
      (\threadId -> killThread threadId >>
        writeIORef interpreterThreadRef Nothing)

  on stepButton #clicked $ do
    set button      [#sensitive := False]
    set stepButton  [#sensitive := False]
    set resetButton [#sensitive := True]
    set nextButton  [#sensitive := True]
    buffer       <- #getBuffer entry
    startIter    <- #getStartIter buffer
    endIter      <- #getEndIter buffer
    text         <- #getText buffer startIter endIter False
    outputBuffer <- #getBuffer outputView
    Gtk.textBufferSetText outputBuffer "" (-1)
    threadId <- forkIO $ void $
      runReaderT (InterpreterGui.run (T.unpack text))
        GUIState { outputView, inputEntry
                 , inputToggle = sendButton
                 , evalState   = StepMode
                 , stepperLock, tapeRef
                 , tapeGrid, nextButton
                 , offsetRef }
    writeIORef interpreterThreadRef (Just threadId)
    pure ()

  on nextButton #clicked $ putMVar stepperLock ()

  -- GUI margins
  #setMarginTop    scrolledWindowEntry 32
  #setMarginBottom scrolledWindowEntry 32
  #setMarginTop    button              32
  #setMarginBottom button              32
  #setMarginTop    resetButton         32
  #setMarginBottom resetButton         32

  -- adding all the elements to boxes for shape and bounding
  hbox      <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  vbox      <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  buttonBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  terminal  <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  set buttonBox [#halign := Gtk.AlignCenter]
  #packStart hbox scrolledWindowEntry True True 32
  #packStart terminal scrolledWindowOutput True True 0
  #packStart terminal inputBox False False 0
  #setSizeRequest inputBox (-1) 32
  #packStart vbox terminal True True 32
  #packStart vbox simVbox True True 32
  #packStart buttonBox lockedButton False False 16
  #packStart buttonBox lockedStepButton False False 16
  #packStart buttonBox lockedNextButton False False 16
  #packStart buttonBox lockedResetButton False False 16
  #packStart vbox buttonBox True True 32
  #packStart hbox vbox True True 32

  -- adding bounding box to main window
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Brainfuck Integrated Development Environment" ]
  #add window hbox

  #showAll window

matchBrackets :: Gtk.TextBuffer -> Gtk.TextTag -> [Gtk.TextTag] -> IO ()
matchBrackets buf errorTag tags = Gtk.textBufferGetStartIter buf >>= loop [] 0
  where apply buf tag iter = Gtk.textIterCopy iter >>=
          \nextIter -> Gtk.textIterForwardChar nextIter >>
          #applyTag buf tag iter nextIter
        loop stack i iter = Gtk.textIterIsEnd iter >>= \e -> unless e $
            Gtk.textIterGetChar iter >>= \case
              '[' -> apply buf errorTag iter >>
                      (:stack) <$> Gtk.textIterCopy iter
              ']' | (openIter:rest) <- stack ->
                      apply buf (tags !! i) openIter >>
                      apply buf (tags !! i) iter  >> pure rest
                  | otherwise -> apply buf errorTag iter >> pure stack
              _ -> pure stack
            >>= \newStack -> Gtk.textIterForwardChar iter >>= \t -> when t $
                loop newStack (succ i `mod` length tags) iter

createButton :: MonadIO m => T.Text -> Bool -> m (Gtk.Button, Gtk.AspectFrame)
createButton label status = do
  button <- new Gtk.Button [ #label     := label
                           , #sensitive := status ]
  lockedButton <- new Gtk.AspectFrame []
  set lockedButton [ #ratio      := 2.0
                   , #obeyChild  := True
                   , #shadowType := Gtk.ShadowTypeNone ]
  #add lockedButton button
  pure (button, lockedButton)

createTag :: MonadIO m => T.Text -> Gtk.TextTagTable -> m Gtk.TextTag
createTag code tagTable = Gtk.textTagNew Nothing >>= \tag ->
  #add tagTable tag >> set tag [ #foreground := code ] >> pure tag

applyTag :: String -> String -> Gtk.TextBuffer -> Gtk.TextTagTable -> IO ()
applyTag colour char buffer table = createTag (T.pack colour) table >>= \tag ->
  let findAndTag iter = #forwardSearch iter (T.pack char)
        [Gtk.TextSearchFlagsVisibleOnly] Nothing >>=
         \(found, matchStart, matchEnd) -> when found $
         #applyTag buffer tag matchStart matchEnd >> findAndTag matchEnd in
  #getStartIter buffer >>= \startIter -> findAndTag startIter

makeTuringSim :: IO ( Gtk.Box
                    , Gtk.Grid
                    , IORef (Tape Word8)
                    , IORef Int
                    , Tape Word8 )
makeTuringSim = do
  simVbox      <- new Gtk.Box  [ #orientation := Gtk.OrientationVertical
                               , #spacing := 10 ]
  simGrid      <- new Gtk.Grid [ #columnSpacing := 10 ]
  simButtonBox <- new Gtk.Box  [ #orientation := Gtk.OrientationHorizontal
                               , #spacing := 10 ]
  (leftButton,  lockedLeftButton ) <- createButton "Left" True
  (rightButton, lockedRightButton) <- createButton "Right" True

  set simGrid [#halign := Gtk.AlignCenter]
  #packStart simButtonBox lockedLeftButton  True True 0
  #packStart simButtonBox lockedRightButton True True 0
  #packStart simVbox simGrid True False 0
  #packStart simVbox simButtonBox False False 0
  let initialTape = Tape (repeat 0) 0 (repeat 0)
  tapeRef <- newIORef initialTape
  offsetRef <- newIORef 0
  drawTape simGrid tapeRef offsetRef

  on leftButton  #clicked $ modifyIORef' offsetRef pred >>
    drawTape simGrid tapeRef offsetRef

  on rightButton #clicked $ modifyIORef' offsetRef succ >>
    drawTape simGrid tapeRef offsetRef
  pure (simVbox, simGrid, tapeRef, offsetRef, initialTape)

run :: IO ()
run = Gtk.applicationNew (Just "foss.brainfuck-ide") [] >>= \case
  Nothing  -> putStrLn "Failed to create the GTK application."
  Just app -> do
    on app #activate (activate app)
    void $ #run app Nothing
