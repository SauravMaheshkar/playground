-- src/Solution110.hs
module Solution110 (testBalanced) where

import Control.Monad (forM)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)

-- Function to check if a given binary tree is balanced
balancedTree :: Tree a -> Bool
balancedTree root = checkBalance root /= -1
  where
    checkBalance :: Tree a -> Int
    checkBalance EmptyTree = 0
    checkBalance (Branch _ left right) =
        let leftHeight = checkBalance left
            rightHeight = checkBalance right
        in if leftHeight == -1 || rightHeight == -1 || abs (leftHeight - rightHeight) > 1
           then -1
           else max leftHeight rightHeight + 1

testBalanced :: FilePath -> IO ()
testBalanced filePath = do
  testCases <- parseJSONFile filePath :: IO [([Maybe Int], Bool)]
  results <- forM testCases $ \(input, expectedOutput) -> do
      let inputTree = constructTree (input)
      let output = balancedTree (inputTree)
      return (output == expectedOutput)
  if (and results)
      then putStrLn "Tests ran successfully for id 110"
      else throwIO $ userError "Tests failed for id 110"
