import Lib
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture_)

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3 -- expected @=? actual
  ,
    testCase "Hello World test" $ do
      let bFile = "test/helloworld.bf"
      output <- capture_ $ run ["-f", bFile]
      assertEqual "Hello World test passed" "Hello, World!\n" output
  ]
