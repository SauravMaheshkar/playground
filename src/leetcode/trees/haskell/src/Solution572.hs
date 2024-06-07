-- src/Solution572.hs
module Solution572 (testSubtree) where

import Control.Monad (forM)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)

-- Function to check if two binary trees are the same
checkSame :: Eq a => Tree a -> Tree a -> Bool
checkSame EmptyTree EmptyTree = True
checkSame EmptyTree _ = False
checkSame _ EmptyTree = False
checkSame (Branch x left1 right1) (Branch y left2 right2) =
    (x == y) && (checkSame left1 left2) && (checkSame right1 right2)

-- Function to check if two binary trees are subtrees of one another
checkSubtree :: Eq a => Tree a -> Tree a -> Bool
checkSubtree _ EmptyTree = True
checkSubtree EmptyTree _ = False
checkSubtree t1@(Branch _ left1 right1) t2 =
    checkSame t1 t2 || checkSubtree left1 t2 || checkSubtree right1 t2


testSubtree :: FilePath -> IO ()
testSubtree filePath = do
  testCases <- parseJSONFile filePath :: IO [([Maybe Int], [Maybe Int], Bool)]
  results <- forM testCases $ \(input_one, input_two, expectedOutput) -> do
      let x_input = constructTree (input_one)
      let y_input = constructTree (input_two)
      let output = checkSubtree x_input y_input
      return (output == expectedOutput)
  if (and results)
      then putStrLn "Tests ran successfully for id 572"
      else throwIO $ userError "Tests failed for id 572"
