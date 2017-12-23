module Main exposing (..)

import DEMO_S5 exposing (..)
import Html exposing (..)
import ExampleModels exposing (..)
import Expression as Expr exposing (Expression)
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
                [ text <| Expr.show sampleExpr
                , text "="
                , text <| toString <| isTrueAt model.model 0 <| sampleExpr
                ]
            , div []
                [ text <| Expr.show <| Expr.Atom Prop.p
                , text "="
                , text <| toString <| isTrueAt model.model 0 <| Expr.Atom Prop.p
                ]
            ]


sampleExpr : Expression a
sampleExpr =
    Expr.Neg <| Expr.Atom Prop.p
