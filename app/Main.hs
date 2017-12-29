module Main where

import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Maybe
import Proto.Carnifex.Configuration

import Lib

main :: IO ()
main = do
  putStrLn "encoding:"
  let decoded = encodePretty dummyContest
  B.putStrLn decoded
  putStrLn "round tripping:"
  let contest = fromJust $ decode dummyJSONContest :: Contest
  B.putStrLn $ encodePretty contest
  putStr "success: "
  print $ decoded == encodePretty contest
