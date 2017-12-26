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
            [ div []
                [ text "After 'At least one red hat': "
                , differentknows initupdatetwo
                , htmlModel initupdatetwo
                , text <| toString <| isTrueAt initupdatetwo 0 (Form.Pub (Form.Conj [ agentdoesntknow Ag.a, agentdoesntknow Ag.b ]) everyoneknows)
                ]
            , div []
                [ text "After 'At least one red hat' and 1 we know nothing: "
                , differentknows afterfirstupdatetwo
                , htmlModel afterfirstupdatetwo
                , text <| toString <| isTrueAt afterfirstupdatetwo 0 (Form.Pub (Form.Conj [ agentdoesntknow Ag.a, agentdoesntknow Ag.b ]) everyoneknows)
                ]
            , div []
                [ text "After 'At least one red hat' and 2 we know nothing: "
                , differentknows secondupdatetwo
                , htmlModel secondupdatetwo
                ]
            ]


htmlModel : EpistM -> Html Msg
htmlModel (Mo sts ag acc val s) =
    div []
        [ div []
            [ text "States: "
            , text <| toString sts
            ]
        , div []
            [ text "Agents: "
            , text <| toString ag
            ]
        , div []
            [ text "Acc: "
            , text <| toString (Acc.toList acc)
            ]
        , div []
            [ text "Val: "
            , text <| toString (Val.toList val)
            ]
        , div []
            [ text "Current state: "
            , text <| toString s
            ]
        ]


differentknows : EpistM -> Html Msg
differentknows model =
    div []
        [ div []
            [ text <| "Noone knows: "
            , text <| toString <| isTrueAt model 0 nooneknows
            ]
        , div []
            [ text <| "Someone knows: "
            , text <| toString <| isTrueAt model 0 someoneknows
            ]
        , div []
            [ text <| "Everyone knows: "
            , text <| toString <| isTrueAt model 0 everyoneknows
            ]
        ]


updatedModel : EpistM -> Int -> EpistM
updatedModel m n =
    case n of
        0 ->
            m

        n ->
            upd_pa (updatedModel (upd_pa m nooneknows) (n - 1)) <| nooneknows
