module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import ExampleModels exposing (..)
import Formula as Form exposing (Formula)
import Prop
import Agent as Ag exposing (Agent)
import AccessRel as Acc
import ValFunction as Val


main =
    Html.program
        { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { model = muddyChildrenTwo }, Cmd.none )



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
        div []
            [ div [] <|
                List.reverse
                    [ muddyModelsUpdatedN model.model
                    ]
            ]
