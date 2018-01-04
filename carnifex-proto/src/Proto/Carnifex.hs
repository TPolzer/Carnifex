{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
module Proto.Carnifex
  ( toTimestamp
  , fromTimestamp
  , toDuration
  , fromDuration
  )
  where

import Control.Lens
import Data.ProtoLens
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Timestamp'Fields
import Proto.Google.Protobuf.Duration

toDiffTime v = fromIntegral (v ^. seconds)
             + fromIntegral (v ^. nanos) / 1e9

fromDiffTime d = def & (seconds .~ s) . (nanos .~ ns)
  where (s, rem) = properFraction d
        ns = round $ rem * 1e9

toTimestamp :: UTCTime -> Timestamp
toTimestamp = fromDiffTime . utcTimeToPOSIXSeconds

fromTimestamp :: Timestamp -> UTCTime
fromTimestamp = posixSecondsToUTCTime . toDiffTime

toDuration :: DiffTime -> Duration
toDuration = fromDiffTime

fromDuration :: Duration -> DiffTime
fromDuration = toDiffTime
