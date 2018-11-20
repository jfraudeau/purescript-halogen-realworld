module Component.Home where

import Prelude

import Api.Endpoint (noArticleParams)
import AppM (Env)
import Capability.LogMessages (class LogMessages)
import Capability.ManageResource (class ManageResource, getArticles, getTags)
import Capability.Now (class Now)
import Control.Monad.Reader (class MonadAsk)
import Data.Article (Article)
import Data.Author (profile, username)
import Data.Const (Const)
import Data.Either (either)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profile (avatarToString)
import Data.Username (toString)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (Array Article)
  }

data Query a
  = LoadTags a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => Now m
  => LogMessages m
  => ManageResource m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
    { initialState: const { tags: NotAsked, articles: NotAsked } 
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ LoadTags unit
    , finalizer: Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query (Const Void) Void Void m
  eval = case _ of
    LoadTags a -> do
      H.modify_ _ { tags = Loading, articles = Loading }
      tags <- H.lift getTags
      H.modify_ _ { tags = either Failure Success tags }
      articles <- H.lift $ getArticles noArticleParams
      H.modify_ _ { articles = either Failure Success articles }
      pure a

  render :: State -> H.ParentHTML Query (Const Void) Void m
  render { tags, articles} =
    HH.div
      [ className "home-page" ]
      [ banner
      , HH.div
          [ classNames [ "container", "page" ] ]
          [ HH.div
              [ className "row" ]
              [ mainView articles
              , HH.div
                  [ className "col-md-3" ]
                  [ HH.div
                      [ className "sidebar" ]
                      [ HH.p_ [ HH.text "Popular Tags" ]
                      , renderTags tags
                      ]
                  ]
              ]
          ]
      ]
  
  banner :: forall p i. HH.HTML p i
  banner =
    HH.div
      [ className "banner" ]
      [ HH.div
          [ className "container" ]
          [ HH.h1
              [ className "logo-font" ]
              [ HH.text "myApp" ]
          , HH.p_ [ HH.text "A place to share your knowledge." ]
          ]
      ]

  mainView :: forall p i. RemoteData String (Array Article) -> HH.HTML p i
  mainView articles =
    HH.div
      [ className "col-md-9" ]
      [ HH.div
          [ className "feed-toggle" ]
          [ HH.ul
              [ classNames [ "nav", "nav-pills", "outline-active" ] ]
              [ yourFeedTab
              , globalFeedTab
              --, tagFilterTab
              ]
          ]
      , articleList articles
      ]

  yourFeedTab :: forall p i. HH.HTML p i
  yourFeedTab =
    HH.li
      [ className "nav-item" ]
      [ HH.a
          [ className "nav-link" ]
          [ HH.text "Your Feed" ]
      ]
  
  globalFeedTab :: forall p i. HH.HTML p i
  globalFeedTab =
    HH.li
      [ className "nav-item" ]
      [ HH.a
          [ classNames [ "nav-link", "active" ] ]
          [ HH.text "Global Feed" ]
      ]
  
  renderTags :: forall p i. RemoteData String (Array String) -> HH.HTML p i
  renderTags = case _ of
    NotAsked ->  HH.div_ [ HH.text "Tags not loaded" ]
    Loading -> HH.div_ [ HH.text "Loading Tags" ]
    Failure err ->  HH.div_ [ HH.text $ "Failed loading tags: " <> err ]
    Success tags ->
      HH.div
        [ className "tag-list" ]
        $ tags <#> (\tag ->
                    HH.a
                    [ classNames [ "tag-default", "tag-pill" ] ]
                    [ HH.text tag ])

  articleList :: forall p i. RemoteData String (Array Article) -> HH.HTML p i
  articleList = case _ of
    NotAsked -> HH.text "Not loaded"
    Loading -> HH.text "Loading"
    Failure err -> HH.text $ "Error loading articles: " <> err
    Success articles -> HH.div_ $ articles <#> article

  article :: forall p i. Article -> HH.HTML p i
  article a =
    HH.div
      [ className "article-preview" ]
      [ HH.div
          [ className "article-meta" ]
          [ HH.a_
              [ HH.img
                  [ HP.src $ a.author # profile # unwrap # _.avatar # maybe "" avatarToString
                  , HP.alt $ toString $ username $ a.author
                  ]
              ]
          , HH.div
              [ className "info" ]
              [ HH.a [ className "author" ] [ HH.text $ toString $ username a.author ]
              , HH.span [ className "date" ] [ HH.text $ format dateFormatter $ a.createdAt ]
              ]
          , HH.div
              [ className "pull-xs-right" ]
              [ HH.button
                  [ classNames [ "btn", "btn-sm", "btn-outline-primary" ] ]
                  [ HH.i [ className "ion-heart" ] []
                  , HH.text "2"
                  ]
              ]
          ]
      , HH.a
          [ className "preview-link" ]
          [ HH.h1_ [ HH.text a.title ]
          , HH.p_ [ HH.text a.description ]
          , HH.span_ [ HH.text "Read more..." ]
          , HH.ul
              [ className "tag-list" ]
              $ a.tagList <#> \tag -> HH.li
                [ classNames [ "tag-default", "tag-pill", "tag-outline" ] ]
                [ HH.text tag ]
          ]
      ]
         
dateFormatter :: List FormatterCommand
dateFormatter = fromFoldable
  [ DayOfWeekNameShort
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , DayOfMonth
  , Placeholder " "
  , YearFull
  ]

className :: forall r i. String -> HH.IProp ( "class" :: String | r ) i
className = HP.class_ <<< H.ClassName


classNames :: forall r i. Array String -> HH.IProp ( "class" :: String | r ) i
classNames = HP.classes <<< map H.ClassName
