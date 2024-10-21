import Lib
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture_)
import Data.List (isPrefixOf)


hackyWorkaround :: String -> String -> String
hackyWorkaround title output = drop 5 (dropWhileStringMatch (title ++ ":") output)
  where dropWhileStringMatch _ [] = []
        dropWhileStringMatch match str@(x:xs)
          | isPrefixOf match str = drop (length match) str
          | otherwise          = dropWhileStringMatch match xs


main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3 -- expected @=? actual
  , testCase "Hello World test" $ do
      bFileData <- readFile "test/helloworld.bf"
      output <- capture_ (run bFileData)
      let cleanOutput = hackyWorkaround "Hello World test" output
      assertEqual "Hello World test passed" "Hello World!\n" cleanOutput
  -- , testCase "0-99 tpassedest" $ do
  --     bFileData <- readFile "test/print0to99.bf"
  --     output <- capture_ (run bFileData)
  --     let cleanOutput = hackyWorkaround "0-99 test" output
  --     assertEqual "0-99 test " "00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n39\n40\n41\n42\n43\n44\n45\n46\n47\n48\n49\n50\n51\n52\n53\n54\n55\n56\n57\n58\n59\n60\n61\n62\n63\n64\n65\n66\n67\n68\n69\n70\n71\n72\n73\n74\n75\n76\n77\n78\n79\n80\n81\n82\n83\n84\n85\n86\n87\n88\n89\n90\n91\n92\n93\n94\n95\n96\n97\n98\n99\n" cleanOutput
  ]

