module MetaLib
  ( deriveProtoJSON
  ) where

import Data.Aeson.TH
import Data.List
import Data.Maybe
import Text.Casing

deriveProtoJSON = deriveJSON defaultOptions
  { fieldLabelModifier = 
    quietSnake
    . dropProtoLensPrefixes
  , unwrapUnaryRecords = True
  , omitNothingFields = True
  }

dropProtoLensPrefixes =
  fromJust . find (notElem '\'') . tails
