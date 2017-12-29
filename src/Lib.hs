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
import Proto.Carnifex.Common
import Proto.Google.Protobuf.Wrappers

instance IsString StringValue where
    fromString s = StringValue $ fromString s
$(deriveProtoJSON ''StringValue)
$(deriveProtoJSON ''Int64Value)

$(deriveProtoJSON ''NullableRepeatedFileRef)
$(deriveProtoJSON ''FileRef)
$(deriveProtoJSON ''Contest)
$(deriveProtoJSON ''Organization'Location)
$(deriveProtoJSON ''Organization)

dummyContest :: Contest
dummyContest = build ((id .~ "a") . (name .~ "contest") . (penaltyTime .~ maxBound) . (startTime .~ "at the end of the universe"))

dummyJSONContest :: B.ByteString
dummyJSONContest = [r|{
    "banner": null,
    "scoreboard_freeze_duration": "",
    "formal_name": "",
    "penalty_time": 9223372036854775807,
    "name": "contest",
    "start_time": "at the end of the universe",
    "id": "a",
    "logo": null,
    "duration": ""
}|]
