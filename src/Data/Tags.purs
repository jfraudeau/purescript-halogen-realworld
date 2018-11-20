module Data.Tags where

import Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)

decodeTags :: Json -> Either String (Array String)
decodeTags json = do
  obj <- decodeJson json
  tags <- obj .? "tags"
  decodeJson tags
