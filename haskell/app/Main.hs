module Main where

import System.Environment (getArgs)
import InterpreterStdio (run)
import Control.Monad (void)
import GUITest (run)

handleArgs :: [String] -> IO ()
handleArgs ("-f":f:_) = void (readFile f >>= InterpreterStdio.run)
handleArgs ("-s":s:_) = void $ InterpreterStdio.run s
handleArgs ("-g":_)   = GUITest.run
handleArgs _ = error "missing or invalid args"

main:: IO()
main = getArgs >>= handleArgs
