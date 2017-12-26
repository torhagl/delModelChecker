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
    ( { model = muddyChildren }, Cmd.none )



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
            [ text <| toString <| isTrueAt afterfirstupdatetwo 0 nooneknows
            , text <| toString <| isTrueAt afterfirstupdatetwo 0 someoneknows
            , text <| toString <| isTrueAt afterfirstupdatetwo 0 aknows
            , text <| toString <| isTrueAt afterfirstupdatetwo 0 bknows
            , text <| toString <| isTrueAt afterfirstupdatetwo 0 everyoneknows
            , text <| toString <| isTrueAt secondupdatetwo 0 nooneknows
            , text <| toString <| isTrueAt secondupdatetwo 0 someoneknows
            , text <| toString <| isTrueAt secondupdatetwo 0 everyoneknows
            , text <| toString <| isTrueAt thirdupdatetwo 0 nooneknows
            , text <| toString <| isTrueAt thirdupdatetwo 0 someoneknows
            , text <| toString <| isTrueAt thirdupdatetwo 0 everyoneknows
            ]


showNupdatedmuddymodel : Formula a -> EpistM -> Int -> Html Msg
showNupdatedmuddymodel formula model n =
    div []
        [ text <| Form.show formula
        , text ", model updated "
        , text <| toString n
        , text " times = "
        , text <| toString <| isTrueAt (updatedModel model n) 0 formula
        ]


updatedModel : EpistM -> Int -> EpistM
updatedModel m n =
    case n of
        0 ->
            m

        n ->
            upd_pa (updatedModel (upd_pa m nooneknows) (n - 1)) <| nooneknows
