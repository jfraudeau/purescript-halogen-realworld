module Component.Page.Login where

import Prelude

import Component.Classes as CC
import Component.HTML.Footer (footer)
import Component.HTML.Header (header)
import Data.Route (Route)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB

render :: forall p f. (Route -> Unit -> f Unit) -> H.HTML p f
render nav =
  HH.div_
  [ header nav
  , authPage
  , footer
  ]

authPage :: forall p i. HH.HTML p i
authPage =
  HH.div
  [ HP.class_ CC.authPage ]
  [ HH.div
    [ HP.classes [ HB.container, CC.page ] ]
    [ ]
  ]
