import Lib
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture_)
import Data.List (isPrefixOf)


hackyWorkaround :: String -> Int -> String -> String
hackyWorkaround title n output = drop n (dropWhileStringMatch (title ++ ":") output)
  where dropWhileStringMatch _ [] = []
        dropWhileStringMatch match str@(x:xs)
          | match `isPrefixOf` str = drop (length match) str
          | otherwise          = dropWhileStringMatch match xs


makeRepetition :: Int -> String
makeRepetition n = [show x | x <- [0..n]] >>= (\x -> (replicate (length (show n) - length x) '0' ++ x) ++ "\n")

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3 -- expected @=? actual
  , testCase "Hello World test" $ do
      bFileData <- readFile "test/helloworld.bf"
      output <- capture_ (run bFileData)
      let cleanOutput = hackyWorkaround "Hello World test" 5 output
      assertEqual "Hello World test passed" "Hello World!\n" cleanOutput
--   , testCase "0-99 test" $ do
--       bFileData <- readFile "test/print0to99.bf"
--       output <- capture_ (run bFileData)
--       let cleanOutput = hackyWorkaround "0-99 test" 12 output
--       assertEqual "0-99 test " (makeRepetition 99) cleanOutput
  -- , testCase "0-999 test" $ do
  --     bFileData <- readFile "test/print0-999.bf"
  --     output <- capture_ (run bFileData)
  --     let cleanOutput = hackyWorkaround "0-999 test" 0 output
  --     assertEqual "0-999 test " (makeRepetition 999) cleanOutput
  ]

