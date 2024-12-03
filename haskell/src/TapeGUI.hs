{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module TapeGUI(drawTape, drawCell) where

import qualified GI.Gtk as Gtk
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Text as T
import Data.GI.Base (on, AttrOp((:=)), new)
import Data.Word (Word8)
import Tape (Tape(..), shiftRight, shiftLeft, inc, dec, store, index)

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
