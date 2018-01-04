module Main where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Maybe
import Data.ProtoLens.Message

import Proto.Carnifex.Configuration
import Proto.Carnifex.JSON
import Lib

roundTrip = encode . fromJust . (decode :: B.ByteString -> Maybe Contest)

main :: IO ()
main = do
  print dummyContest
  putStrLn "test:"
  let decoded = encode dummyContest
  B.putStrLn decoded
  putStrLn "round tripping:"
  let contest = fromJust $ decode $ encodePretty dummyContest :: Contest
  B.putStrLn $ encodePretty contest
  putStr "success: "
  print $ decoded == encodePretty contest
  let contest = fromJust $ decode dummyJSONContest :: Contest
  print contest
  B.putStrLn $ encodePretty contest
  B.interact (B.concat . map roundTrip . B.lines)
