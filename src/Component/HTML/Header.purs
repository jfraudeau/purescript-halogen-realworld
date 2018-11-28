module Component.HTML.Header where

import Prelude

import Component.HTML.Utils (css)
import Data.Monoid (guard)
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Our header will be a pure render function, but we'll require a route as an 
-- argument so we can judge whether a link should display active or not. We'll 
-- also take a query so that we can trigger navigation actions from the HTML. 

header :: forall i p. Route -> (Route -> H.Action p) -> HH.HTML i (p Unit)
header route navigate =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
      [ css "container" ]
      [ HH.button
        [ css "navbar-brand" 
        , HE.onClick $ HE.input_ $ navigate Home
        ]
        [ HH.text "conduit" ]
    , HH.ul
      [ css "nav navbar-nav pull-xs-right" ]
      [ navItem Home 
      , navItem Editor
      , navItem Settings
      , navItem Register
      ]
      ]
    ]

  where

  navItem r = 
    HH.li
      [ css "nav-item" ]
      [ HH.button
        [ css $ "nav-link" <> guard (route == r) " active"
        , HE.onClick $ HE.input_ $ navigate r
        ]
        [ displayRoute r ]
      ]

  displayRoute = case _ of
    Home -> 
      HH.text "Home"
    Editor -> 
      HH.i 
        [ css "ion-compose" ]
        [ HH.text " New Post" ]
    Settings -> 
      HH.i
        [ css "ion-gear-a" ]
        [ HH.text " Settings" ]
    Register -> 
      HH.text "Sign up"
    x -> 
      HH.text $ show x
