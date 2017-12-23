module AccessRel exposing (..)

import Dict exposing (Dict)
import Agent exposing (..)
import State exposing (..)


type AccessRel
    = AccessRel (Dict Agent (List EqClass))


type alias EqClass =
    List State


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
    case list of
        [] ->
            empty

        ( ag, eq ) :: xs ->
            insert ag eq <| fromList xs
