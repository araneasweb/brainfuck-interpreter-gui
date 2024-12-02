{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module TapeGUI where

import qualified GI.Gtk as Gtk
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Text as T
import Data.GI.Base (on, AttrOp((:=)), new)
import Data.Word (Word8)
import Tape (Tape(..), shiftRight, shiftLeft, inc, dec, store, index)

main :: IO ()
main = do
    -- Initialize GTK
    _ <- Gtk.init Nothing

    -- Create the main application window
    window <- new Gtk.Window [ #title := "Turing Tape", #defaultWidth := 600, #defaultHeight := 200 ]

    -- Create a vertical box layout
    vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10 ]
    #add window vbox

    -- Create a grid for the tape
    grid <- new Gtk.Grid []
    #packStart vbox grid True True 0

    -- Tape state: using the actual tape datatype
    -- We're assuming that the tape is passed to this in such a way that
    --   its modifications don't pass to the interpreter
    let initialTape = Tape (repeat 0) 1 (repeat 0) :: Tape Word8
    tapeRef <- newIORef initialTape

    -- Draw the initial tape
    drawTape grid tapeRef

    -- Create a horizontal box for buttons
    hbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10 ]
    #packStart vbox hbox False False 0

    -- Navigation buttons
    btnLeft <- new Gtk.Button [ #label := "Left" ]
    btnRight <- new Gtk.Button [ #label := "Right" ]
    #packStart hbox btnLeft True True 0
    #packStart hbox btnRight True True 0

    -- Button actions
    _ <- on btnLeft #clicked $ do
        modifyIORef' tapeRef shiftLeft
        drawTape grid tapeRef

    _ <- on btnRight #clicked $ do
        modifyIORef' tapeRef shiftRight
        drawTape grid tapeRef

    -- Close application on window destroy
    _ <- on window #destroy Gtk.mainQuit

    -- Show all widgets
    #showAll window
    Gtk.main

-- Draw the tape on the grid
drawTape :: Gtk.Grid -> IORef (Tape Word8) -> IO ()
drawTape grid tapeRef = do
    tape <- readIORef tapeRef

    -- Clear the grid by destroying its children
    children <- Gtk.containerGetChildren grid
    mapM_ Gtk.widgetDestroy children

    let windowSize = 11
        index = div (windowSize - 1) 2
        Tape prev i suc = tape
        cells = reverse (take index prev) ++ [i] ++ take (windowSize - index - 1) suc

    -- Draw each tape cell
    mapM_ (uncurry (drawCell grid index)) (zip cells [0..(windowSize - 1)])

    #showAll grid

-- Draw a single cell
drawCell :: Gtk.Grid -> Int -> Word8 -> Int -> IO ()
drawCell grid currentIdx content pos = do
    let labelText = T.pack (show content)
    label <- new Gtk.Label [ #label := labelText ]

    -- Highlight the active cell
    if pos == currentIdx
        then Gtk.widgetSetName label "active-cell"
        else Gtk.widgetSetName label "normal-cell"

    Gtk.gridAttach grid label (fromIntegral pos) 0 1 1
    #show label
