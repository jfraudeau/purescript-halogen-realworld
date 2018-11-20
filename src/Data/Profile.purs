module Data.Profile
  ( Avatar -- No constructors exported
  , mkAvatar
  , avatarToString
  , Profile(..)
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?), (.?=), (.??))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Username (Username)

-- Our Profile entity will represent information necessary to render any user 
-- profile in the in the system, including the currently-authenticated one. We'll 
-- use a newtype rather than a type synonym over a record so that we can write 
-- JSON instances for the type.

newtype Profile = Profile
  { username :: Username
  , bio :: Maybe String
  , avatar :: Maybe Avatar
  }

derive instance newtypeProfile :: Newtype Profile _
derive instance eqProfile :: Eq Profile

instance encodeJsonProfile :: EncodeJson Profile where
  encodeJson (Profile pr) = "username" := pr.username ~> jsonEmptyObject

instance decodeJsonProfile :: DecodeJson Profile where
  decodeJson js = do
    obj <- decodeJson js
    username <- obj .? "username"
    bio <- obj .?? "bio" .?= Nothing
    avatar <- obj .?? "image" .?= Nothing
    pure $ Profile {username, bio, avatar }

-- We'll represent avatars using the smart constructor pattern. Avatars are not 
-- required, but if there is one associated with a user then it cannot be empty. 
-- We'll represent "no avatar" with the `Maybe` type instead of the empty string.

newtype Avatar = Avatar String

derive instance genericAvatar :: Generic Avatar _
derive instance eqAvatar :: Eq Avatar

derive newtype instance encodeJsonAvatar :: EncodeJson Avatar
derive newtype instance decodeJsonAvatar :: DecodeJson Avatar

instance showAvatar :: Show Avatar where
  show = genericShow

-- For now we'll just verify that if an avatar is meant to exist it is at least 
-- a non-empty string, but as we grow the app we might put more stringent 
-- requirements in place, like requiring a valid URL.

mkAvatar :: String -> Maybe Avatar
mkAvatar "" = Nothing
mkAvatar str = Just (Avatar str)

avatarToString :: Avatar -> String
avatarToString (Avatar str) = str
