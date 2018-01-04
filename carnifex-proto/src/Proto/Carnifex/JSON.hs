{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}
module Proto.Carnifex.JSON
  ()
  where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad (mzero)
import Control.DeepSeq
import Data.Either
import Data.Text (pack, unpack)
import Data.Time.ISO8601
import Data.Time.Clock
import Data.Scientific
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Wrappers
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Text.Format
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Carnifex.Reltime
import Proto.Carnifex
import Proto.Carnifex.Configuration
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
parseTime = withText "expected a date" $
  maybe mzero return . parseISO8601 . unpack

parseDuration :: Value -> Parser DiffTime
parseDuration = withText "expected a reltime" $ \t ->
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

instance NFData Int64Value
instance NFData StringValue
instance NFData FileRef
instance NFData Timestamp
instance NFData Duration
instance NFData Contest

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

instance FromJSON StringValue where
  parseJSON = addAttributes $(mkParseIcpcJSON ''StringValue)

instance ToJSON StringValue where
  toJSON = filterAttributes . $(mkToIcpcJSON ''StringValue)

instance FromJSON Int64Value where
  parseJSON = addAttributes $(mkParseIcpcJSON ''Int64Value)

instance ToJSON Int64Value where
  toJSON = filterAttributes . $(mkToIcpcJSON ''Int64Value)

instance FromJSON TaggedValue where
  parseJSON _ = mzero

instance ToJSON TaggedValue where
  toJSON _ = Null
