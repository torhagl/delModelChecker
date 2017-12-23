module DEMO_S5 exposing (..)

import Expression exposing (..)
import Agent exposing (Agent)
import Prop exposing (Prop)
import ValFunction as Val exposing (ValFunction)
import AccessRel as Acc exposing (EqClass, AccessRel)
import State exposing (..)


type EpistM
    = Mo (List State) (List Agent) AccessRel ValFunction State


empty : EpistM
empty =
    Mo [] [] Acc.empty Val.empty 0


isTrueAt : EpistM -> State -> Expression a -> Bool
isTrueAt model st expression =
    let
        (Mo sts ags rel val _) =
            model
    in
        case expression of
            Top ->
                True

            Atom p ->
                List.member p <|
                    case Val.get st val of
                        Just a ->
                            a

                        Nothing ->
                            []

            Neg subf ->
                not <| isTrueAt model st subf

            Disj listOfDisj ->
                List.any (isTrueAt model st) listOfDisj

            Conj listOfConj ->
                List.all (isTrueAt model st) listOfConj
