module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile)
import Solution100 (testSame)
import Solution104 (testMaximumDepth)
import Solution110 (testBalanced)
import Solution226 (testInvertTree)
import Solution543 (testDiameter)
import Solution572 (testSubtree)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dataPath] -> do
            putStrLn "Testing Haskell implementation for Tree problems"
            let basePath = dataPath ++ "/"
            testInvertTree (basePath ++ "226.json")
            testMaximumDepth (basePath ++ "104.json")
            testDiameter (basePath ++ "543.json")
            testBalanced (basePath ++ "110.json")
            testSame (basePath ++ "100.json")
            testSubtree (basePath ++ "572.json")
        _ -> putStrLn "Usage: program <data_path>"
