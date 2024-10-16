module Main where

import System.Environment (getArgs)
import Lib (run)

main:: IO()
main = getArgs >>= run

