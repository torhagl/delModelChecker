module DEMO_S5 exposing (..)

import Dict exposing (..)


type alias Agent =
    String


type Prop
    = P Int
    | Q Int
    | R Int
    | S Int
    | Pred Agent Int -- Works as a predicate with closed world assumption. Handy in some situations.


type Form a
    = Top
    | Prp Prop
    | Ng (Form a)
    | Disj (List (Form a))


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
