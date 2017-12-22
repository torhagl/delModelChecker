module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import Dict exposing (..)
import Formulae as Form exposing (Formulae)
import Prop
import ExampleModels exposing (..)


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


view : Model -> Html Msg
view model =
    let
        (Mo states agents accesRel valFunc currentState) =
            model.model
    in
        div [] <| List.map text <| List.map Form.show <| formulaeTrueInState model.model currentState


formulaeTrueInState : EpistM -> State -> List (Formulae a)
formulaeTrueInState (Mo _ _ _ valFunc _) st =
    case Dict.get st valFunc of
        Just props ->
            List.map Form.Prp props

        Nothing ->
            []
