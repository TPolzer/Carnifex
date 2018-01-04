{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Maybe
import Data.List
import Data.ProtoLens.Message
import Control.Parallel.Strategies

import Proto.Carnifex.Configuration
import Proto.Carnifex.JSON
import Lib

de = fromJust . (decode :: B.ByteString -> Maybe Contest)
p :: (NFData a) => [a] -> [a]
p = (`using` parBuffer 64 rdeepseq)

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
  B.interact (B.concat . intersperse "\n" . p . map encode . p . map de . B.lines)
  B.putStrLn ""
