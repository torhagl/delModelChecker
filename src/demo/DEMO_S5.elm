module DEMO_S5 exposing (..)

import Dict exposing (..)
import Formulae exposing (..)
import Agent exposing (Agent)
import Prop exposing (Prop)
import ValFunction as Val
import AccessRel as Acc


type EpistM
    = Mo (List State) (List Agent) AccessRel ValFunction State


empty : EpistM
empty =
    Mo [ 0 ] [ "a" ] Acc.empty Val.empty 0


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
