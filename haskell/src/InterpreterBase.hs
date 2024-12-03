module InterpreterBase(Interpreter(..), run, InterpreterState(..)) where
import Data.Binary (Word8)
import Data.Map (Map, lookup, insert, empty)
import Control.Monad (void)
import Tape (Tape(..), shiftRight, shiftLeft, inc, dec, store, index)

-- | A data structure storing the state for evaluation of a given piece of brainfuck source code. Has:
-- * @readingIndex@, an @Int@ index of the source code at the current evaluation step
-- * @bracketMap@, a @Map Int Int@ lookup table for brackets and their pairs
-- * @sourceCode@, a @String@ of the brainfuck source code, with comments
-- * @tape@, a @Tape Word8@ of the current memory state of the system
data InterpreterState = InterpreterState
  { readingIndex :: Int
  , bracketMap   :: Map Int Int
  , sourceCode   :: String
  , tape         :: Tape Word8 }

-- | A monad that defines the IO actions that would happen with the interpteter
--
-- It has 3 methods:
-- * @recurseStandby@, called immediately prior to each recursive call
--   @[(Int -> Map Int Int -> String -> Tape Word8 -> m (Tape Word8)) -> InterpreterState -> m (Tape Word8)]@
-- * @writeInputFromTape@, called when the interpreter wants to write something to the tty @[Tape Word8 -> m (Tape Word8)]@
-- * @writeInputToTape@, called when the interpreter wants a user input (single character in) @[Tape Word8 -> m (Tape Word8)]@
class Monad m => Interpreter m where
  recurseStandby :: (Int -> Map Int Int -> String -> Tape Word8 -> m (Tape Word8)) -> InterpreterState -> m (Tape Word8)
  writeInputFromTape :: Tape Word8 -> m (Tape Word8)
  writeInputToTape :: Tape Word8 -> m (Tape Word8)


-- If we want to have good (efficient) bracket jumps, we want to make a lookup table where a given index of a bracket is matched
--   with the index of its partner. This should be autopopulated.
-- We can have an index counter that recursively increases as we parse the string that we can outright change for jumps
-- Technically this is slower than a jump table with saved states, but it's both WAY easier to implement and notably more memory efficient
-- | Given a string, produces a @Just (Map Int Int)@ of each bracket and its partner (and vice versa), or Nothing if there is a broken pair
buildBracketMap :: String -> Maybe (Map Int Int) -- would love to find a way to not need to precompute but my brain is too small
buildBracketMap s = buildMap s 0 [] empty
  where
    buildMap [] _ [] k = Just k
    buildMap [] _ _ _ = Nothing
    buildMap (x:xs) i stack k
      | x == '[' = buildInc (i:stack) k
      | x == ']' = case stack of
          [] -> Nothing
          (p:ps) -> buildInc ps (insert p i $ insert i p k)
      | otherwise = buildInc stack k
      where buildInc = buildMap xs (i + 1)


-- we want to be able to take in a String and be able to do the operations requested
-- If the operation is a '[', store the location of the string at the current moment
--    If the value at the index is currently 0, jump directly to the paired '[', otherwise continue
-- If the operation is a ']', store the location of the string at the current moment
--    If the value at the index is currently nonzero, jump directly to ']', otherwise continue
-- If the operation is a '.', output the current index of the table into the tty
-- If the operation is a ',' request a single char input from the tty
-- If the operation is a '<', shiftLeft the tape
-- If the operation is a '>', shiftRight the tape
-- If the operation is a '+', `inc` the table at the current index
-- If the operation is a '-', `dec` the table at the current index
parseString :: Interpreter m => Maybe (Map Int Int) -> Tape Word8 -> String -> m (Tape Word8)
parseString Nothing _ _ = error "mismatched braces"
parseString (Just bracketMap) tape string = parseChars 0 bracketMap string tape
  where
    parseChars :: Interpreter m => Int -> Map Int Int -> String -> Tape Word8 -> m (Tape Word8)
    parseChars i m s t -- get it because
      | i >= length s = return t
      | otherwise = case s !! i of
          '.' -> writeInputFromTape t >>= parseInc
          ',' -> writeInputToTape t >>= parseInc
          '<' -> parseInc (shiftLeft t)
          '>' -> parseInc (shiftRight t)
          '+' -> parseInc (inc t)
          '-' -> parseInc (dec t)
          '[' | index t == 0 -> jump
              | otherwise    -> parseInc t
          ']' | index t /= 0 -> jump
              | otherwise    -> parseInc t
          _   -> parseInc t -- should never happen
      where
        parseInc t = recurseStandby parseChars InterpreterState { readingIndex = i+1
                                                                , bracketMap   = m
                                                                , sourceCode   = s
                                                                , tape         = t }
        jump = case Data.Map.lookup i m of
                Just j  -> recurseStandby parseChars InterpreterState { readingIndex = j+1
                                                                      , bracketMap   = m
                                                                      , sourceCode   = s
                                                                      , tape         = t }
                Nothing -> error "mismatched braces" -- should never happen

pruneString :: String -> String
pruneString [] = []
pruneString (x:xs) | x `notElem` ".,<>+-[]" = pruneString xs
                   | otherwise              = x : pruneString xs

-- | runs the interpreter with an empty tape and initialised bracketmap
run :: Interpreter m => String -> m (Tape Word8)
run s = parseString (buildBracketMap ps) (Tape (repeat 0) 0 (repeat 0)) ps
  where ps = pruneString s


