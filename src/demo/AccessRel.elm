module AccessRel exposing (..)

import Dict exposing (Dict)
import Agent exposing (..)
import State exposing (..)


type AccessRel
    = AccessRel (Dict Agent (List EqClass))


type alias EqClass =
    List State


getEqClass : Agent -> State -> AccessRel -> Maybe EqClass
getEqClass ag st acc =
    case get ag acc of
        Just eqclasses ->
            List.head <| List.filter (List.member st) eqclasses

        Nothing ->
            Nothing


empty : AccessRel
empty =
    AccessRel Dict.empty


get : Agent -> AccessRel -> Maybe (List EqClass)
get ag (AccessRel rel) =
    case Dict.get ag rel of
        Just a ->
            Just a

        Nothing ->
            Nothing


insert : Agent -> List EqClass -> AccessRel -> AccessRel
insert ag eqclass (AccessRel acc) =
    AccessRel <| Dict.insert ag eqclass acc


fromList : List ( Agent, List EqClass ) -> AccessRel
fromList list =
    AccessRel <| Dict.fromList list


toList : AccessRel -> List ( Agent, List EqClass )
toList (AccessRel acc) =
    Dict.toList acc


map : (Agent -> List EqClass -> List EqClass) -> AccessRel -> AccessRel
map f (AccessRel acc) =
    AccessRel <| Dict.map f acc


mapWithoutKey : (List EqClass -> List EqClass) -> AccessRel -> AccessRel
mapWithoutKey f (AccessRel acc) =
    AccessRel <| Dict.foldl (\ag v newdict -> Dict.insert ag (f v) newdict) Dict.empty acc
