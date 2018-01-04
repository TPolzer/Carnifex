{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}
module Proto.Carnifex.JSON
  ()
  where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad (mzero)
import Data.Text (pack, unpack)
import Data.Time.ISO8601
import Data.Time.Clock
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Wrappers

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
addAttributes convert (Object o) = convert $ Object $ o `HashMap.union` additionalFields where
  repeatedFields :: [FieldDescriptor msg]
  repeatedFields = filter isRepeatedField $ Map.elems fieldsByTag
  repeatedFieldNames = map (pack . fieldDescriptorName) repeatedFields
  additionalFields = HashMap.fromList
    [(k,Array mzero) | k <- unknown:repeatedFieldNames]
addAttributes convert v = convert $ 
  Object $ HashMap.fromList [(unknown, empty), (field, v)] where
    field = pack $ fieldDescriptorName $
      (head $ Map.elems fieldsByTag :: FieldDescriptor msg)

parseTime :: Value -> Parser UTCTime
parseTime = withText "expected a date" $
  maybe mzero return . parseISO8601 . unpack

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
