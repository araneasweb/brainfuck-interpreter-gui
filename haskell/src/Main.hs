module Main where
import Data.Binary (Word8)
import System.Environment (getArgs)
import Data.Map (Map, lookup, insert, empty)
import Control.Monad (void)

-- We want to have a tape datatype where we can get the current index, the prev elements and the suc elements
-- The succ elements are a normal list, but the prev elements are in reverse order so head gets the previously element and (:) adds it back (O(1) hopefully)
-- It should have a function to shift forward by one element
-- It should have a function to shift back by one element
-- It should have a function to increment the index by 1
-- It should have a function to decrement the index by 1
-- It should have a function to return the current index
-- The prev and suc elements are infinitely populated with zeros, so it should never be the case that either one is the empty list
-- This is generalised but since we want an 8 bit number in each cell, we will use a Words8
data Tape a = Tape [a] a [a]
    deriving (Show)

shiftRight :: (Bounded a) => Tape a -> Tape a
shiftRight (Tape prev i []) = Tape (i:prev) minBound (repeat minBound)
shiftRight (Tape prev i (a:as)) = Tape (i:prev) a as

shiftLeft :: Bounded a => Tape a -> Tape a
shiftLeft (Tape [] i suc) = Tape (repeat minBound) minBound (i:suc)
shiftLeft (Tape (a:as) i suc) = Tape as a (i:suc)

inc :: (Eq a, Bounded a, Enum a) => Tape a -> Tape a
inc (Tape prev i suc)
  | i == maxBound = Tape prev minBound suc
  | otherwise = Tape prev (succ i) suc

dec :: (Eq a, Bounded a, Enum a) => Tape a -> Tape a
dec (Tape prev i suc)
  | i == minBound = Tape prev maxBound suc
  | otherwise = Tape prev (pred i) suc

store :: a -> Tape a -> Tape a
store x (Tape prev _ suc) = Tape prev x suc
index :: Tape a -> a
index (Tape _ i _) = i


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

-- THIS IS WRONG, I didn't realise you can jump forwards too
-- Saved states should be stored in a stack-equivalent datatype
-- Store the index of the current position relative to the base string (e.g. as a (String, Int))
-- Once an operation is complete, remove that character from the string
-- the state storing should be fine because haskell is lazy and wouldn't straight up copy everything

-- THIS IS BETTER
-- I made a mistake here, and if I want to have good (efficient) bracket jumps I want to make a lookup table where a given index of a bracket is matched
--   with the index of its partner. This should be autopopulated and used instead of the stack. (which is sad because i really liked my stack implementation)
-- We can have an index counter that recursively increases as we parse the string that we can outright change for jumps
-- Technically this is slower than a jump table with saved states, but it's both WAY easier to implement and notably more memory efficient
charToByte :: Char -> Maybe Word8
charToByte c
  | conv <= 255 = Just$fromIntegral conv
  | otherwise = Nothing
  where conv = fromEnum c

-- monads are kinda like the fae yk

writeInputToTape :: Tape Word8 -> IO (Tape Word8)
writeInputToTape t = getChar >>= \c -> case charToByte c of
  Just c -> return (store c t)
  Nothing -> error "invalid character read"

toChar :: Word8 -> Char
toChar = toEnum.fromIntegral

writeInputFromTape :: Tape Word8 -> IO (Tape Word8)
writeInputFromTape t = putChar (toChar$index t) >> return t

buildBracketMap :: String -> Maybe (Map Int Int) -- would love to find a way to not need to precompute but my brain is too small
buildBracketMap s = buildMap s 0 [] empty
  where
    buildMap [] _ [] k = Just k
    buildMap [] _ _ _ = Nothing
    buildMap (x:xs) i stack k
      | x == '[' = buildInc (i : stack) k
      | x == ']' = case stack of
          [] -> Nothing
          (p:ps) -> buildInc ps (insert p i $ insert i p k)
      | otherwise = buildInc stack k
      where buildInc = buildMap xs (i + 1)

parseString :: Maybe (Map Int Int) -> Tape Word8 -> String -> IO (Tape Word8)
parseString Nothing _ _ = error "mismatched braces"
parseString (Just m) tape s = parseChars 0 tape
  where
    parseChars i t -- get it because
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
          _   -> parseInc t -- might be more efficient to just prune comments at the beginning (before bracketmap too)
      where
        parseInc = parseChars (i + 1)
        jump = case Data.Map.lookup i m of
                Just j  -> parseChars (j + 1) t
                Nothing -> error "mismatched braces" -- should never happen

handleArgs :: [String] -> IO String
handleArgs ("-f":f:_) = readFile f
handleArgs ("-s":s:_) = return s
handleArgs _ = error "missing or invalid args"

main:: IO()
main = getArgs >>= handleArgs >>= \s -> void (parseString (buildBracketMap s) (Tape (repeat 0) 0 (repeat 0)) s)





---- DEPRICATED CODE ----

-- newtype Stack a = Stack [a]
--     deriving (Show)

-- push :: a -> Stack a -> Stack a
-- push x (Stack xs) = Stack (x:xs)

-- pop :: Stack a -> (Maybe a, Stack a)
-- pop (Stack []) = (Nothing, Stack [])
-- pop (Stack (x:xs)) = (Just x, Stack xs)

-- bracketMapToLookupTable :: String -> Maybe (M.Map Int Int) -> Maybe (M.Map Int String)
-- bracketMapToLookupTable _ Nothing = Nothing
-- bracketMapToLookupTable s (Just m) = Just $ M.map (`drop` s) (m + 1)

-- parseChar :: Char -> Tape Word8 -> IO (Tape Word8)
-- parseChar c
--   | c == '.' = writeInputFromTape
--   | c == ',' = writeInputToTape
--   | c == '<' = return.shiftLeft
--   | c == '>' = return.shiftRight
--   | c == '+' = return.inc
--   | c == '-' = return.dec
--   | otherwise = return

-- parseString :: Stack String -> Tape Word8 -> String -> IO (Tape Word8)
-- parseString _ t [] = return t
-- parseString s t (x:xs)
--   | x == '[' = parseString (push xs s) t xs
--   | x == ']' = case pop s of
--     (Just str, s') -> parseString s' t str
--     (Nothing, _) -> error "mismatched closing brace"
--   | otherwise = parseChar x t >>= \t' -> parseString s t' xs

