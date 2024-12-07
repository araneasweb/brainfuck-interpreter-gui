-- |Module: Tape
module Tape(Tape (..), shiftRight, shiftLeft, inc, dec, store, index) where

-- We want to have a tape datatype where we can get the current index, the prev elements and the suc elements
-- The succ elements are a normal list, but the prev elements are in reverse order so head gets the previously element and (:) adds it back (O(1) hopefully)
-- It should have a function to shift forward by one element
-- It should have a function to shift back by one element
-- It should have a function to increment the index by 1
-- It should have a function to decrement the index by 1
-- It should have a function to return the current index
-- The prev and suc elements are infinitely populated with zeros, so it should never be the case that either one is the empty list
-- This is generalised but since we want an 8 bit number in each cell, we will use a Words8 in implementation


-- |  @Tape [a] a [a]@
--
-- A tape datatype with 3 parts:
--
-- * An infinite list of the preceeding elements (or zeros)
-- * The index
-- * An infinite list of the succeeding elements (or zeros)
data Tape a = Tape [a] a [a]
    deriving (Show)

-- | A function to shift a tape rightwards, not modifying its data
shiftRight :: (Bounded a) => Tape a -> Tape a
shiftRight (Tape prev i []) = Tape (i:prev) minBound (repeat minBound)
shiftRight (Tape prev i (a:as)) = Tape (i:prev) a as

-- | A function to shift a tape leftwards, not modifying its data
shiftLeft :: Bounded a => Tape a -> Tape a
shiftLeft (Tape [] i suc) = Tape (repeat minBound) minBound (i:suc)
shiftLeft (Tape (a:as) i suc) = Tape as a (i:suc)

-- | A function to increment the data at the index of a tape by 1
inc :: (Eq a, Bounded a, Enum a) => Tape a -> Tape a
inc (Tape prev i suc)
  | i == maxBound = Tape prev minBound suc
  | otherwise = Tape prev (succ i) suc

-- | A function to decrement the data at the index of a tape by 1
dec :: (Eq a, Bounded a, Enum a) => Tape a -> Tape a
dec (Tape prev i suc)
  | i == minBound = Tape prev maxBound suc
  | otherwise = Tape prev (pred i) suc

-- | A function to store some value @a@ at the tape's index
store :: a -> Tape a -> Tape a
store x (Tape prev _ suc) = Tape prev x suc

-- | A function to read the data at a tape's index
index :: Tape a -> a
index (Tape _ i _) = i
