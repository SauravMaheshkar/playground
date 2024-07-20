-- src/eval.hs
module Eval (Tree (EmptyTree, Branch), constructTree, parseJSONFile) where

import Data.Aeson (FromJSON(..), Value(..), eitherDecode, withArray)
import Data.Vector (toList)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as BL

-- Tree datatype
data Tree a = EmptyTree | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- Ref: https://stackoverflow.com/a/60518030/14081966
constructTree :: [a] -> Tree a
constructTree xs = head nodes
  where
  nodes = zipWith g (map Just xs ++ repeat Nothing)
                    (pairs $ tail nodes)
  g (Just x) (lt,rt)  =  Branch x lt rt
  g Nothing  _        =  EmptyTree
  pairs ~(a: ~(b:c))  =  (a,b) : pairs c

-- TestCase datatypes
newtype TreeTreeTestCase = TreeTreeTestCase ([Int], [Int])
newtype TreeIntTestCase = TreeIntTestCase ([Maybe Int], Int)
newtype TreeBoolTestCase = TreeBoolTestCase([Maybe Int], Bool)
newtype TreeTreeBoolTestCase = TreeTreeBoolTestCase ([Maybe Int], [Maybe Int], Bool)

instance FromJSON TreeTreeTestCase where
    parseJSON = withArray "TreeTreeTestCase" $ \arr ->
        case toList arr of
            [inputArray, outputArray] -> do
                input <- parseJSON inputArray
                output <- parseJSON outputArray
                return $ TreeTreeTestCase (input, output)
            _ -> fail "Expected a pair of lists"

instance FromJSON TreeIntTestCase where
    parseJSON = withArray "TreeIntTestCase" $ \arr ->
        case toList arr of
            [inputArray, Number n] -> do
                input <- parseJSON inputArray
                let output = round n
                return $ TreeIntTestCase (input, output)
            _ -> fail "Expected a list and a number"

instance FromJSON TreeBoolTestCase where
    parseJSON = withArray "TreeBoolTestCase" $ \arr ->
        case toList arr of
            [inputArray, Bool b] -> do
                input <- parseJSON inputArray
                return $ TreeBoolTestCase (input, b)
            _ -> fail "Expected a list and a boolean"

instance FromJSON TreeTreeBoolTestCase where
    parseJSON = withArray "TreeTreeBoolTestCase" $ \arr ->
        case toList arr of
            [inputArray1, inputArray2, Bool b] -> do
                input1 <- parseJSON inputArray1
                input2 <- parseJSON inputArray2
                return $ TreeTreeBoolTestCase (input1, input2, b)
            _ -> fail "Expected two lists and a boolean"

-- Parse the JSON file into a list of input-output pairs
parseJSONFile :: FromJSON a => FilePath -> IO [a]
parseJSONFile filePath = do
    content <- BL.readFile filePath
    case eitherDecode content of
        Right pairs -> return pairs
        Left err    -> error $ "Failed to parse JSON: " ++ err
