module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import ExampleModels exposing (..)
import Formula as Form exposing (Formula)
import Prop


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
        div []
            [ div []
                [ text <| Form.show sampleExpr
                , text "="
                , text <| toString <| isTrueAt model.model 1 <| sampleExpr
                ]
            , div []
                [ text <| Form.show <| Form.Atom Prop.p
                , text "="
                , text <| toString <| isTrueAt model.model 1 <| Form.Atom Prop.p
                ]
            ]


sampleExpr : Formula a
sampleExpr =
    Form.Neg <| Form.Atom Prop.p
