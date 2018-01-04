{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Lib
    (dummyContest
    , dummyJSONContest
    ) where

import Data.List
import Data.Maybe
import Control.Lens
import Data.ProtoLens.Message
import Data.Aeson.TH
import Prelude hiding (id)
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as B
import Data.String

import MetaLib
import Proto.Carnifex.Configuration as Configuration
import Proto.Carnifex.Configuration'Fields as Configuration
import Proto.Google.Protobuf.Wrappers

dummyContest :: Contest
dummyContest = build ((id .~ "a") . (name .~ "contest") . (penaltyTime .~ maxBound) {-. (startTime .~ "at the end of the universe")-})

dummyJSONContest :: B.ByteString
dummyJSONContest = [r|{
    "scoreboard_freeze_duration": 1,
    "penalty_time": 2147483647,
    "name": "contest",
    "start_time": "2018-01-03T19:56:27+00:00",
    "id": "a",
    "duration": 2
}|]
