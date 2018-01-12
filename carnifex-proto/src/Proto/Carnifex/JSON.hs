{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell, ExistentialQuantification, FlexibleContexts, AllowAmbiguousTypes, DataKinds, StandaloneDeriving, DeriveGeneric #-}
module Proto.Carnifex.JSON
  () -- only export JSON instances of all relevant types
  where

import Data.Aeson
import Data.Aeson.Types
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Either
import Data.Hashable
import Data.Int
import Data.Maybe
import Data.ProtoLens
import Data.Text (pack, unpack)
import Data.Time.ISO8601 -- TODO rip this out if performance becomes a concern, SLOW AS HELL
import Data.Time.Clock
import Data.Tuple
import Data.Scientific
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Wrappers
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Text.Format -- TODO transition to maintained 'formatting' instead of 'text-format'
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.Generics (Generic)
import Lens.Labels (HasLens')
import Prelude hiding (id)

import Carnifex.Reltime
import Proto.Carnifex
import Proto.Carnifex.Configuration
import Proto.Carnifex.Live
import Proto.Carnifex.Scoreboard
import Proto.Carnifex.EventFeed
import Proto.Carnifex.EventFeed'Fields
import Proto.Carnifex.JSON.TH
import Data.ProtoLens.Message

isRepeatedField (FieldDescriptor _ _ (RepeatedField _ _)) = True
isRepeatedField _ = False

unknown = "unknown_fields"
empty = Array mzero

filterAttributes :: Value -> Value
filterAttributes (Object o) =
  if HashMap.size contents == 1
    then head $ HashMap.elems contents
    else Object contents where
      contents = HashMap.filterWithKey keepValue o
filterAttributes v = v
keepValue k v = k /= unknown && v /= empty

addAttributes :: forall msg . (Message msg) => (Value -> Parser msg) -> Value -> Parser msg
addAttributes convert (Object o) = convert $ Object $
    filteredObject `HashMap.union` additionalFields where
  repeatedFields :: [FieldDescriptor msg]
  repeatedFields = filter isRepeatedField $ Map.elems fieldsByTag
  repeatedFieldNames = map (pack . fieldDescriptorName) repeatedFields
  additionalFields = HashMap.fromList
    [(k,Array mzero) | k <- unknown:repeatedFieldNames]
  filteredObject = HashMap.filter (/=Null) o
addAttributes convert v = convert $ 
  Object $ HashMap.fromList [(unknown, empty), (field, v)] where
    field = pack $ fieldDescriptorName $
      (head $ Map.elems fieldsByTag :: FieldDescriptor msg)

parseTime :: Value -> Parser UTCTime
parseTime = withText "TIME" $
  maybe mzero return . parseISO8601 . unpack

parseDuration :: Value -> Parser DiffTime
parseDuration = withText "RELTIME" $ \t ->
  either (const mzero) return $
    parseOnly (durationParser <* endOfInput) t

formatDiffTime :: DiffTime -> T.Text
formatDiffTime d = LT.toStrict $ format "{}{}:{}:{}.{}" (sign, h, fm, fs, fus) where
  sign = if d < 0 then "-" else "" :: LT.Text
  truncated = abs $ round $ d*1e3 :: Int
  (hms, us) = abs truncated `quotRem` 1000
  (hm, s) = hms `quotRem` 60
  (h, m) = hm `quotRem` 60
  fm = left 2 '0' m
  fs = left 2 '0' s
  fus = left 3 '0' us

eventOpToEnum :: HashMap.HashMap T.Text Event'Operation
eventOpToEnum = HashMap.fromList
  [ ("create", Event'CREATE)
  , ("update", Event'UPDATE)
  , ("delete", Event'DELETE)
  ]

instance Hashable Event'Operation'UnrecognizedValue
instance Hashable Event'Operation
eventEnumToOp :: HashMap.HashMap Event'Operation T.Text
eventEnumToOp = HashMap.fromList $ map swap $ HashMap.toList eventOpToEnum

eventDataParser :: HashMap.HashMap T.Text (Bool -> Value -> Parser Event'Data)
eventDataParser = HashMap.fromList
  [ ("contests",        (fmap Event'Contest.) . parse) 
  , ("judgement-types", (fmap Event'JudgementType.) . parse)
  , ("problems",        (fmap Event'Problem.) . parse)
  , ("groups",          (fmap Event'Group.) . parse)
  , ("organizations",   (fmap Event'Organization.) . parse)
  , ("teams",           (fmap Event'Team.) . parse)
  , ("state",           (fmap Event'ContestState.) . const parseJSON) -- does not have id...
  , ("submissions",     (fmap Event'Submission.) . parse)
  , ("judgements",      (fmap Event'Judgement.) . parse)
  ] where parse delete = if delete then parseIdJSON else parseJSON

parseIdJSON :: forall out . (Default out, Lens.Labels.HasLens' Identity out "id" T.Text) => Value -> Parser out
parseIdJSON = fmap (\pId -> (def :: out) & (id .~ pId)) . (.: "id") <=< parseJSON

data EventData = forall m . (Message m, ToJSON m) => EventData m
instance ToJSON EventData where
  toJSON (EventData m) = toJSON m

formatEventData :: Event'Data -> [Pair]
formatEventData d = ["type" .= type', "data" .= dat] where
  tag (Event'Contest m) = ("contests" :: T.Text, EventData m)
  tag (Event'JudgementType m) = ("judgement-types", EventData m)
  tag (Event'Problem m) = ("problems", EventData m)
  tag (Event'Group m) = ("groups", EventData m)
  tag (Event'Organization m) = ("organizations", EventData m)
  tag (Event'Team m) = ("teams", EventData m)
  tag (Event'ContestState m) = ("state", EventData m)
  tag (Event'Submission m) = ("submissions", EventData m)
  tag (Event'Judgement m) = ("judgements", EventData m)
  (type', dat) = tag d

-- TODO Apply TH Magic to reduce boilerplate
deriving instance Generic Int32Value
deriving instance Generic StringValue
deriving instance Generic BoolValue
deriving instance Generic FileRef
deriving instance Generic Timestamp
deriving instance Generic Duration
deriving instance Generic Contest
deriving instance Generic JudgementType
deriving instance Generic Problem
deriving instance Generic Group
deriving instance Generic Organization
deriving instance Generic Team
deriving instance Generic ContestState
deriving instance Generic ScoreboardRow
deriving instance Generic Score
deriving instance Generic ScoredProblem
deriving instance Generic Submission
deriving instance Generic Judgement
deriving instance Generic Event
deriving instance Generic Event'Operation
deriving instance Generic Event'Operation'UnrecognizedValue
deriving instance Generic Event'Data
instance NFData Int32Value
instance NFData StringValue
instance NFData BoolValue
instance NFData FileRef
instance NFData Timestamp
instance NFData Duration
instance NFData Contest
instance NFData JudgementType
instance NFData Problem
instance NFData Group
instance NFData Organization
instance NFData Team
instance NFData ContestState
instance NFData ScoreboardRow
instance NFData Score
instance NFData ScoredProblem
instance NFData Submission
instance NFData Judgement
instance NFData Event
instance NFData Event'Operation
instance NFData Event'Operation'UnrecognizedValue
instance NFData Event'Data

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> do
    parsedId <- v .: "id"
    rawType <- v .: "type"
    rawOp <- v .: "op"
    rawData <- v .: "data"
    let (Just parsedOp) = HashMap.lookup rawOp eventOpToEnum
    let (Just parser) = ($ parsedOp == Event'DELETE) <$> HashMap.lookup rawType eventDataParser
    parsedData <- Just <$> parser rawData
    return $! def & (id .~ parsedId) . (operation .~ parsedOp) . (maybe'data' .~ parsedData)

instance ToJSON Event where
  toJSON e = object $ "id" .= (e ^. id) : opData ++ eventData where
    eventData = maybeToList $ ("op" .=) <$> HashMap.lookup (e ^. operation) eventEnumToOp
    opData = concat $ formatEventData <$> (e ^. maybe'data')

instance FromJSON Submission where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Submission)

instance ToJSON Submission where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Submission)

instance FromJSON Judgement where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Judgement)

instance ToJSON Judgement where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Judgement)

instance FromJSON ScoreboardRow where
  parseJSON = addAttributes $(mkParseIcpcJSON ''ScoreboardRow)

instance ToJSON ScoreboardRow where
  toJSON = filterAttributes . $(mkToIcpcJSON ''ScoreboardRow)

instance FromJSON Score where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Score)

instance ToJSON Score where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Score)

instance FromJSON ScoredProblem where
  parseJSON = addAttributes $(mkParseIcpcJSON ''ScoredProblem)

instance ToJSON ScoredProblem where
  toJSON = filterAttributes . $(mkToIcpcJSON ''ScoredProblem)

instance FromJSON ContestState where
  parseJSON = addAttributes $(mkParseIcpcJSON ''ContestState)

instance ToJSON ContestState where
  toJSON = filterAttributes . $(mkToIcpcJSON ''ContestState)

instance FromJSON JudgementType where
  parseJSON = addAttributes $(mkParseIcpcJSON ''JudgementType)

instance ToJSON JudgementType where
  toJSON = filterAttributes . $(mkToIcpcJSON ''JudgementType)

instance FromJSON Problem where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Problem)

instance ToJSON Problem where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Problem)

instance FromJSON Group where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Group)

instance ToJSON Group where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Group)

instance FromJSON Organization where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Organization)

instance ToJSON Organization where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Organization)

instance FromJSON Team where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Team)

instance ToJSON Team where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Team)

instance FromJSON Duration where
  parseJSON v = toDuration <$> parseDuration v

instance ToJSON Duration where
  toJSON = String . formatDiffTime . fromDuration

instance FromJSON Timestamp where
  parseJSON v = toTimestamp <$> parseTime v

instance ToJSON Timestamp where
  toJSON = String . pack . formatISO8601 . fromTimestamp

instance FromJSON Contest where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Contest)

instance ToJSON Contest where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Contest)

instance FromJSON FileRef where
  parseJSON = addAttributes $(mkParseIcpcJSON ''FileRef)

instance ToJSON FileRef where
  toJSON = filterAttributes . $(mkToIcpcJSON ''FileRef)

instance FromJSON BoolValue where
  parseJSON = addAttributes $(mkParseIcpcJSON ''BoolValue)

instance ToJSON BoolValue where
  toJSON = filterAttributes . $(mkToIcpcJSON ''BoolValue)

instance FromJSON StringValue where
  parseJSON = addAttributes $(mkParseIcpcJSON ''StringValue)

instance ToJSON StringValue where
  toJSON = filterAttributes . $(mkToIcpcJSON ''StringValue)

instance FromJSON Int32Value where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Int32Value)

instance ToJSON Int32Value where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Int32Value)

instance FromJSON TaggedValue where
  parseJSON _ = mzero

instance ToJSON TaggedValue where
  toJSON _ = Null -- TODO LOG If this is ever called
