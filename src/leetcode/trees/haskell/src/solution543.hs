-- src/solution543.hs
module Solution543 (testDiameter) where

import Control.Monad (forM)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)

-- Function to calculate the maximum depth of a binary tree
maximumDepth :: Tree a -> Int
maximumDepth EmptyTree = 0
maximumDepth (Branch _ left right) = 1 + max (maximumDepth left) (maximumDepth right)

-- Function to calculate the diameter of a binary tree
diameter :: Tree a -> Int
diameter EmptyTree = 0
diameter (Branch _ left right) =
    let leftDepth = maximumDepth left
        rightDepth = maximumDepth right
    in max (leftDepth + rightDepth) (max (diameter left) (diameter right))

testDiameter :: FilePath -> IO ()
testDiameter filePath = do
  testCases <- parseJSONFile filePath :: IO [([Maybe Int], Int)]
  results <- forM testCases $ \(input, expectedOutput) -> do
      let inputTree = constructTree (input)
      let output = diameter (inputTree)
      return (output == expectedOutput)
  if (and results)
      then putStrLn "Tests ran successfully for id 543"
      else throwIO $ userError "Tests failed for id 543"
