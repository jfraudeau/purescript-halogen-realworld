module Component.HTML.Header where

import Prelude

import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB


-- TODO: links, show "new post" and "settings" only for logged user
header :: forall p f. (Route -> H.Action f) -> H.HTML p f
header navigate =
  HH.nav
  [ HP.classes [ HB.navbar, HB.navbarLight ] ]
  [ HH.div
    [ HP.class_ HB.container ]
    [ HH.a
      [ HP.class_ HB.navbarBrand ]
      [ HH.text "conduit" ]
    , HH.ul
      [ HP.classes
        [ HB.nav, HB.navbarNav, HH.ClassName "pull-xs-right" ]
      ]
      [ HH.li
        [ HP.class_ HB.navItem ]
        [ HH.a
          [ HP.classes [ HB.navLink, HB.active ]
          , HE.onClick $ HE.input_ $ navigate Home
          ]
          [ HH.text "Home" ]
        ]
      , HH.li
        [ HP.class_ HB.navItem ]
        [ HH.a
          [ HP.classes [ HB.navLink ] ]
          [ HH.i [ HP.class_ $ HH.ClassName "ion-compose" ] []
          , HH.text "\160New Post"
          ]
        ]
      , HH.li
        [ HP.class_ HB.navItem ]
        [ HH.a
          [ HP.classes [ HB.navLink ] ]
          [ HH.i [ HP.class_ $ HH.ClassName "ion-gear-a" ] []
          , HH.text "\160Settings"
          ]
        ]
      , HH.li
        [ HP.class_ HB.navItem ]
        [ HH.a
          [ HP.classes [ HB.navLink ], HE.onClick $ HE.input_ $ navigate Login ]
          [ HH.text "Sign up" ]
        ]
      ]
    ]
  ]
