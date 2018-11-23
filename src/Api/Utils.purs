module Api.Utils
       (withUser
       , withAuthUser
       , withAuthUser_
       ) where

import Prelude

import Affjax (Request)
import Api.Request (AuthUser, BaseURL, runRequest, username)
import Capability.Authenticate (class Authenticate, deleteAuth, readAuth)
import Capability.LogMessages (class LogMessages, logError)
import Capability.Navigate (class Navigate, logout, navigate)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (Json)
import Data.Bifoldable (bitraverse_)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(..))
import Data.Route as Route
import Data.Username (Username)
import Effect.Aff.Class (class MonadAff)

-- Helper functions that leverages several of our capabilities together to help
-- run requests that require authentication.

withUser
  :: forall m e a
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk { rootUrl :: BaseURL | e } m
  => Authenticate m
  => (Username -> Json -> Either String a)
  -> (BaseURL -> Request Json)
  -> m (Either String a)
withUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> pure (Left err)
    Right au -> do
      { rootUrl } <- ask
      runRequest (decode (username au)) $ req rootUrl

withAuthUser 
  :: forall m a e
   . MonadAff m 
  => LogMessages m
  => Navigate m 
  => MonadAsk { rootUrl :: BaseURL | e } m
  => Authenticate m 
  => (Username -> Json -> Either String a)
  -> (AuthUser -> BaseURL -> Request Json)
  -> m (Either String a)
withAuthUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> deleteAuth *> navigate Route.Login *> pure (Left err) 
    Right au -> do
      { rootUrl } <- ask
      res <- runRequest (decode (username au)) (req au rootUrl)
      void $ ltraverse (\e -> logError e *> logout) res
      pure res

withAuthUser_ 
  :: forall m e
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk { rootUrl :: BaseURL | e } m
  => Authenticate m 
  => (AuthUser -> BaseURL -> Request Json) 
  -> m Unit 
withAuthUser_ req =
  readAuth >>= bitraverse_
    (\e -> logError e *> logout)
    (\au -> ask >>= runRequest pure <<< req au <<< _.rootUrl)
