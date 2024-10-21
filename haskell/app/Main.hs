module Main where

import System.Environment (getArgs)
import Lib (run)
import Control.Monad (void)
import GUITest (run)

handleArgs :: [String] -> IO ()
handleArgs ("-f":f:_) = void (readFile f >>= Lib.run)
handleArgs ("-s":s:_) = void $ Lib.run s
handleArgs ("-g":_)   = GUITest.run
handleArgs _ = error "missing or invalid args"

main:: IO()
main = getArgs >>= handleArgs

