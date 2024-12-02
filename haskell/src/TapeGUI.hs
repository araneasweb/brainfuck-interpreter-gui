module TapeGUI where
{-# LANGUAGE OverloadedStrings #-}


import qualified GI.Gtk as Gtk
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Text as T



main :: IO ()
main = do
    -- Initialize GTK
    Gtk.init Nothing

    -- Create the main application window
    window <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.windowSetTitle window "Turing Tape"
    Gtk.windowSetDefaultSize window 600 200

    -- Create a vertical box layout
    vbox <- Gtk.boxNew Gtk.OrientationVertical 10
    Gtk.containerAdd window vbox

    -- Create a grid for the tape
    grid <- Gtk.gridNew
    Gtk.boxPackStart vbox grid True True 0

    -- Tape state: list of symbols and the current head position
    let initialTape = replicate 5 "0" ++ ["1"] ++ replicate 5 "0"
    tapeRef <- newIORef (initialTape, 5)

    -- Draw the initial tape
    drawTape grid tapeRef

    -- Create a horizontal box for buttons
    hbox <- Gtk.boxNew Gtk.OrientationHorizontal 10
    Gtk.boxPackStart vbox hbox False False 0

    -- Navigation buttons
    btnLeft <- new Gtk.Button [#label     := "Left" ]
    btnRight <- new Gtk.Button [#label     := "Right" ]
    Gtk.boxPackStart hbox btnLeft True True 0
    Gtk.boxPackStart hbox btnRight True True 0

    -- Button actions
    _ <- Gtk.onButtonClicked btnLeft $ do
        modifyIORef' tapeRef (moveTape (-1))
        drawTape grid tapeRef

    _ <- Gtk.onButtonClicked btnRight $ do
        modifyIORef' tapeRef (moveTape 1)
        drawTape grid tapeRef

    -- Close application on window destroy
    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    -- Show all widgets
    Gtk.widgetShowAll window
    Gtk.main

-- Move the tape head
moveTape :: Int -> ([String], Int) -> ([String], Int)
moveTape direction (tape, idx) = (tape, max 0 (min (length tape - 1) (idx + direction)))

-- Draw the tape on the grid
drawTape :: Gtk.Grid -> IORef ([String], Int) -> IO ()
drawTape grid tapeRef = do
    (tape, idx) <- readIORef tapeRef

    -- Clear the grid by destroying its children
    children <- Gtk.containerGetChildren grid
    mapM_ Gtk.widgetDestroy children

    -- Draw each tape cell
    mapM_ (uncurry (drawCell grid idx)) (zip tape [0 ..])

-- Draw a single cell
drawCell :: Gtk.Grid -> Int -> String -> Int -> IO ()
drawCell grid currentIdx content pos = do
    let labelText = T.pack content
    label <- Gtk.labelNew (Just labelText)

    -- Highlight the active cell
    if pos == currentIdx
        then Gtk.widgetSetName [label := "active-cell"]
        else Gtk.widgetSetName [label  := "normal-cell"]

    Gtk.gridAttach grid label pos 0 1 1