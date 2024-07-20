-- src/solution104.hs
module Solution104 (testMaximumDepth) where

import Control.Monad (forM)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)

-- Function to calculate the maximum depth of a binary tree
maximumDepth :: Tree a -> Int
maximumDepth EmptyTree = 0
maximumDepth (Branch _ left right) = 1 + max (maximumDepth left) (maximumDepth right)

testMaximumDepth :: FilePath -> IO ()
testMaximumDepth filePath = do
  testCases <- parseJSONFile filePath :: IO [([Maybe Int], Int)]
  results <- forM testCases $ \(input, expectedOutput) -> do
      let inputTree = constructTree (input)
      let output = maximumDepth (inputTree)
      return (output == expectedOutput)
  if (and results)
      then putStrLn "Tests ran successfully for id 104"
      else throwIO $ userError "Tests failed for id 104"
