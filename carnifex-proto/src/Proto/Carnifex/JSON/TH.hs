module Proto.Carnifex.JSON.TH
  (mkToIcpcJSON, mkParseIcpcJSON)
  where

import Language.Haskell.TH.Syntax
import Data.Aeson.TH
import Text.Casing
import Data.Maybe
import Data.List

mkToIcpcJSON = mkToJSON icpcOptions
mkParseIcpcJSON = mkParseJSON icpcOptions

icpcOptions = defaultOptions
  { fieldLabelModifier = 
    quietSnake
    . dropProtoLensPrefixes
  , unwrapUnaryRecords = True
  , omitNothingFields = True
  }

dropProtoLensPrefixes =
  fromJust . find (notElem '\'') . tails
