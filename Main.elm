module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import Dict exposing (..)


main =
    Html.program
        { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { model = exampleModel }, Cmd.none )



-- MODEL


type alias Model =
    { model : EpistM }



-- UPDATE


type Msg
    = Hello


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hello ->
            ( { model | model = exampleModel }, Cmd.none )


exampleModel : EpistM
exampleModel =
    Mo [ 0, 1 ] [ "a", "b" ] (Dict.fromList [ ( "a", [ [ 0 ], [ 1 ] ] ), ( "b", [ [ 0, 1 ] ] ) ]) (Dict.fromList [ ( 0, [ P 1 ] ), ( 1, [ P 2 ] ) ]) 0


view : Model -> Html Msg
view model =
    let
        (Mo states _ _ _ _) =
            model.model
    in
        div [] <| List.map text <| List.map toString states
