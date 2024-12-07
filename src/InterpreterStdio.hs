{-# LANGUAGE InstanceSigs #-}
-- |Module: InterpreterStdio
module InterpreterStdio(run) where
import Data.Binary (Word8)
import Data.Map (Map, empty, insert, lookup)
import InterpreterBase (Interpreter (..), InterpreterState (..), run)
import Tape (Tape, index, store)
-- wrapper on InterpreterBase such that it works with stdio :)

charToByte :: Char -> Maybe Word8
charToByte c
  | conv <= 255 = Just$fromIntegral conv
  | otherwise = Nothing
  where conv = fromEnum c

toChar :: Word8 -> Char
toChar = toEnum.fromIntegral

instance Interpreter IO where
  recurseStandby :: (Int -> Map Int Int -> String -> Tape Word8 -> IO (Tape Word8)) -> InterpreterState -> IO (Tape Word8)
  recurseStandby f InterpreterState { readingIndex = readingIndex
                                    , bracketMap   = bracketMap
                                    , sourceCode   = sourceCode
                                    , tape         = tape
                                    } = f readingIndex bracketMap sourceCode tape

  writeInputFromTape :: Tape Word8 -> IO (Tape Word8)
  writeInputFromTape t = putChar (toChar $ index t) >> return t

  writeInputToTape :: Tape Word8 -> IO (Tape Word8)
  writeInputToTape t = getChar >>= \c -> case charToByte c of
    Just c -> return (store c t)
    Nothing -> error "invalid character read"
