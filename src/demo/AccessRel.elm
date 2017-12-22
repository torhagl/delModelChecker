module AccessRel exposing (..)

import Dict exposing (..)
import Agent exposing (..)


type alias AccessRel =
    Maybe (Dict Agent (List EqClass))


type alias EqClass =
    List Int


empty : AccessRel
empty =
    Nothing


get : Agent -> AccessRel -> Maybe (List EqClass)
get ag rel =
    case rel of
        Just acc ->
            case Dict.get ag rel of
                Just a ->
                    Just a

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


insert : Agent -> List EqClass -> AccessRel -> AccessRel
insert ag eqclass acc =
    case acc of
        Just rel ->
            Just <| Dict.insert ag eqclass rel

        Nothing ->
            Just <| Dict.insert ag eqclass Dict.empty
