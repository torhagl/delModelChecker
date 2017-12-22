module DEMO_S5 exposing (..)

import Dict exposing (..)
import Formulae exposing (..)
import Agent exposing (Agent)
import Prop exposing (Prop)


type alias State =
    Int


type alias AccessRel =
    Dict Agent (List EqClass)


type alias ValFunction =
    Dict State (List Prop)


type alias EqClass =
    List Int


type EpistM
    = Mo (List State) (List Agent) AccessRel ValFunction State


rel : EpistM -> Agent -> List EqClass
rel (Mo _ _ accRel _ _) agent =
    case Dict.get agent accRel of
        Just a ->
            a

        Nothing ->
            []


propsTrueInState : EpistM -> State -> List Prop
propsTrueInState (Mo _ _ _ val _) st =
    case Dict.get st val of
        Just a ->
            a

        Nothing ->
            []


isTrueAt : EpistM -> State -> Formulae a -> Bool
isTrueAt model st formulae =
    let
        (Mo sts ags rel val _) =
            model
    in
        case formulae of
            Top ->
                True

            Prp p ->
                List.member p <| propsTrueInState model st

            Ng subf ->
                not <| isTrueAt model st subf

            Disj listOfDisj ->
                List.any (isTrueAt model st) listOfDisj
