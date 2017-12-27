module HtmlDemo exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import Bootstrap.Card as Card


htmlModel : EpistM -> Html Msg
htmlModel model =
    div [] []
