{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module TapeGUI(drawTape, drawCell) where

import qualified GI.Gtk as Gtk
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Text as T
import Data.GI.Base (on, AttrOp((:=)), new)
import Data.Word (Word8)
import Tape (Tape(..), shiftRight, shiftLeft, inc, dec, store, index)

drawTape :: Gtk.Grid -> IORef (Tape Word8) -> IORef Int -> IO ()
drawTape grid tapeRef offsetRef = do
    tape <- readIORef tapeRef
    offset <- readIORef offsetRef

    -- Clear the grid by destroying its children
    children <- Gtk.containerGetChildren grid
    mapM_ Gtk.widgetDestroy children

    let windowSize = 11
        index = div (windowSize - 1) 2
        Tape prev i suc = shiftTape offset tape
        cells = reverse (take index prev) ++ [i] ++ take (windowSize - index - 1) suc

    -- Draw each tape cell
    mapM_ (uncurry (drawCell grid index offset)) (zip cells [0..(windowSize - 1)])

    #showAll grid

drawCell :: Gtk.Grid -> Int -> Int -> Word8 -> Int -> IO ()
drawCell grid currentIdx offset content pos = do
    let labelText = T.pack (show content)
    label <- new Gtk.Label [ #label := labelText ]

    if pos == currentIdx - offset
        then Gtk.set label [ #label := T.concat ["[", labelText, "]"] ]
        else Gtk.set label [ #label := labelText ]

    Gtk.gridAttach grid label (fromIntegral pos) 0 1 1
    #show label

shiftTape :: Int -> Tape Word8 -> Tape Word8
shiftTape 0 tape = tape
shiftTape n tape
    | n > 0 = shiftTape (n - 1) (shiftRight tape)
    | n < 0 = shiftTape (n + 1) (shiftLeft  tape)
