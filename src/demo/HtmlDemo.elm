module HtmlDemo exposing (..)

import DEMO_S5 exposing (..)
import State exposing (State)
import Agent exposing (Agent)
import AccessRel as Acc exposing (EqClass, AccessRel)
import ValFunction as Val exposing (ValFunction)
import Html exposing (..)
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


htmlModel : EpistM -> Html msg
htmlModel model =
    let
        (Mo sts ags acc val current) =
            model
    in
        Grid.row []
            [ Grid.col [] [ htmlStates sts ]
            ]


htmlStates : List State -> Html msg
htmlStates sts =
    Card.config [ Card.outlinePrimary ]
        |> Card.headerH5 [] [ text "States" ]
        |> Card.block []
            [ Card.titleH6 [] [ text "Title" ] ]
        |> Card.view
