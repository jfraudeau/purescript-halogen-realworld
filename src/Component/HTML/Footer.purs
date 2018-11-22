module Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB

footer :: forall p i. HH.HTML p i
footer =
  HH.footer_
  [ HH.div
    [ HP.class_ HB.container ]
    [ HH.a
      [ HP.class_ (HH.ClassName "logo-font") ]
      [ HH.text "conduit" ]
    , HH.span
      [ HP.class_ (HH.ClassName "attribution") ]
      [ HH.text "An interactive learning project from "
      , HH.a
        [ HP.href "https://thinkster.io" ]
        [ HH.text "Thinkster" ]
      , HH.text ". Code & design licensed under MIT."
      ]
    ]
  ]
