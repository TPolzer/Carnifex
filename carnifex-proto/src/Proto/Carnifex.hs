module Proto.Carnifex
  ( toTimestamp
  , fromTimestamp
  )
  where

import Control.Lens
import Data.ProtoLens
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Timestamp'Fields

toTimestamp :: UTCTime -> Timestamp
toTimestamp time = def & (seconds .~ s) . (nanos .~ ns)
  where diff = utcTimeToPOSIXSeconds time
        (s, rem) = properFraction diff
        ns = round $ rem / 10^9

fromTimestamp :: Timestamp -> UTCTime
fromTimestamp timestamp = posixSecondsToUTCTime diff
  where
    diff = fromIntegral (timestamp ^. seconds)
           + fromIntegral (timestamp ^.nanos) / 10^9
