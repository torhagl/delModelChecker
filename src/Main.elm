module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import ExampleModels exposing (..)
import HtmlDemo exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Card as Card
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


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
        (Mo states agents accessRel valFunc currentState) =
            model.model
    in
        Grid.container []
            [ CDN.stylesheet
            , htmlModel model.model
            , muddyTwoModel
            ]
