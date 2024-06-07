-- src/Solution226.hs
module Solution226 (testInvertTree) where

import Control.Monad (forM)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)

-- Function to invert the tree
invertTree :: Tree a -> Tree a
invertTree EmptyTree = EmptyTree
invertTree (Branch value left right) =
  let invertedLeft = invertTree left
      invertedRight = invertTree right
  in Branch value invertedRight invertedLeft

testInvertTree :: FilePath -> IO ()
testInvertTree filePath = do
  testCases <- parseJSONFile filePath :: IO [([Int], [Int])]
  results <- forM testCases $ \(input, output) -> do
      let inputTree = constructTree (input)
      let expectedOutput = constructTree (output)
      let outputTree = invertTree (inputTree)
      return (outputTree == expectedOutput)
  if (and results)
      then putStrLn "Tests ran successfully for id 226"
      else throwIO $ userError "Tests failed for id 226"
