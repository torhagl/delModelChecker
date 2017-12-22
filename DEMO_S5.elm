module DEMO_S5 exposing (..)

import Dict exposing (..)
import Formulae exposing (..)
import Agent exposing (..)
import Prop exposing (..)


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


agentEqClasses : Agent -> EpistM -> List EqClass
agentEqClasses agent (Mo _ _ accRel _ _) =
    case Dict.get agent accRel of
        Just a ->
            a

        Nothing ->
            []
