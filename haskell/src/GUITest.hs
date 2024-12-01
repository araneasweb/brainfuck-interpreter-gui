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
import qualified GI.Gdk as Gdk



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


  buffer <- #getBuffer entry
  

  on buffer #changed $ do 
    tagTable <- #getTagTable buffer

    -- memory leak here?

    commentTag <- Gtk.textTagNew Nothing
    set commentTag [ #foreground := "grey" ]
    _ <- #add tagTable commentTag
    startIter <- #getStartIter buffer
    endIter <- #getEndIter buffer
    #applyTag buffer commentTag startIter endIter

    -- plus and minus

    plusTag <-  Gtk.textTagNew Nothing
    set plusTag [#foreground := "#dc8a78"]  
    _ <- #add tagTable plusTag
    findCharAndApplyTag buffer (T.pack "+") plusTag

    minusTag <- Gtk.textTagNew Nothing
    _ <- #add tagTable minusTag
    set minusTag [ #foreground := "#dc8a78" ]
    findCharAndApplyTag buffer (T.pack "-") minusTag

    -- rangle and langle

    rAngleTag <- Gtk.textTagNew Nothing
    _ <- #add tagTable rAngleTag
    set rAngleTag [#foreground := "#7287fd"]  
    findCharAndApplyTag buffer (T.pack ">") rAngleTag

    lAngleTag <- Gtk.textTagNew Nothing
    _ <- #add tagTable lAngleTag
    set lAngleTag [#foreground := "#7287fd"]  
    findCharAndApplyTag buffer (T.pack "<") lAngleTag

    -- period and comma

    periodTag <- Gtk.textTagNew Nothing
    _ <- #add tagTable periodTag
    set periodTag [ #foreground := "#8839ef" ]
    findCharAndApplyTag buffer (T.pack ".") periodTag

    commaTag <- Gtk.textTagNew Nothing
    _ <- #add tagTable commaTag
    set commaTag [ #foreground := "#8839ef" ]
    findCharAndApplyTag buffer (T.pack ",") commaTag

    errorTag <- Gtk.textTagNew Nothing 
    _ <- #add tagTable errorTag
    set errorTag [#foreground := "red"]


    -- set bracketTag [#foreground := "#d20f39"] 
    matchBrackets buffer tagTable errorTag ["#d20f39", "#df8e1d", "#40a02b", "#04a5e5", "#7287fd"]




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

findCharAndApplyTag :: Gtk.TextBuffer -> T.Text -> Gtk.TextTag -> IO ()
findCharAndApplyTag buffer char tag = do
    startIter <- #getStartIter buffer
    let findAndTag iter = do
            (found, matchStart, matchEnd) <- #forwardSearch iter char [Gtk.TextSearchFlagsVisibleOnly] Nothing
            if found
                then do
                    #applyTag buffer tag matchStart matchEnd
                    -- continue searching from the end of the match
                    findAndTag matchEnd
                else return ()  

    findAndTag startIter


-- highlight matching pairs of brackets, red if unpaired
matchBrackets :: Gtk.TextBuffer -> Gtk.TextTagTable -> Gtk.TextTag -> [String] -> IO ()
matchBrackets buffer tagTable errorTag colours = do
    iter <- Gtk.textBufferGetStartIter buffer

    let loop currentIter stack i = do
      -- check if we are at the end
            isEnd <- Gtk.textIterIsEnd currentIter
            if not isEnd
                then do
                    char <- Gtk.textIterGetChar currentIter
                    -- update the stack
                    stack' <- 
                      if char == '['
                        then do
                            -- copied2 highlights the error
                            copiedIter1 <- Gtk.textIterCopy currentIter
                            copiedIter2 <- Gtk.textIterCopy currentIter
                            _ <- Gtk.textIterForwardChar copiedIter2
                            #applyTag buffer errorTag currentIter copiedIter2
                            return (copiedIter1 : stack)
                        else if char == ']'
                            then do
                                case stack of
                                    (lastIter:rest) -> do
                                        -- create a new tag and add to the match
                                        -- need to copy a bunch of stuff to avoid modification
                                        copiedIter <- Gtk.textIterCopy lastIter
                                        _ <- Gtk.textIterForwardChar copiedIter
                                        bracketTag <- Gtk.textTagNew Nothing
                                        _ <- #add tagTable bracketTag
                                        set bracketTag [#foreground := (T.pack (colours !! i))] 

                                        #applyTag buffer bracketTag lastIter copiedIter

                                        copiedIter <- Gtk.textIterCopy currentIter
                                        _ <- Gtk.textIterForwardChar copiedIter
                                        #applyTag buffer bracketTag currentIter copiedIter

                                        -- pop off
                                        return rest
                                    [] -> do 
                                      -- too many ]
                                      copiedIter <- Gtk.textIterCopy currentIter
                                      _ <- Gtk.textIterForwardChar copiedIter
                                      #applyTag buffer errorTag currentIter copiedIter
                                      return stack 
                        else return stack
                    -- traverse the text
                    moved <- Gtk.textIterForwardChar currentIter
                    if moved
                        then loop currentIter stack' (mod (i + 1) (length colours))
                        else return ()
            else return ()

    -- Start the search loop
    loop iter [] 0


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
