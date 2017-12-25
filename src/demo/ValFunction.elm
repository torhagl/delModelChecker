module ValFunction exposing (..)

import State exposing (..)
import Dict exposing (Dict)
import Prop exposing (..)
import Agent as Ag exposing (Agent)


type ValFunction
    = ValFunction (Dict State (List Prop))


empty : ValFunction
empty =
    ValFunction Dict.empty


get : State -> ValFunction -> Maybe (List Prop)
get st (ValFunction v) =
    case Dict.get st v of
        Just a ->
            Just a

        Nothing ->
            Nothing


insert : State -> List Prop -> ValFunction -> ValFunction
insert st props (ValFunction v) =
    ValFunction <| Dict.insert st props v


fromList : List ( State, List Prop ) -> ValFunction
fromList list =
    ValFunction <| Dict.fromList list


toList : ValFunction -> List ( State, List Prop )
toList (ValFunction val) =
    Dict.toList val


filter : (State -> List Prop -> Bool) -> ValFunction -> ValFunction
filter f (ValFunction val) =
    ValFunction <| Dict.filter f val


partition : (State -> List Prop -> Bool) -> ValFunction -> ( ValFunction, ValFunction )
partition f (ValFunction val) =
    Tuple.mapSecond ValFunction <| Tuple.mapFirst ValFunction <| Dict.partition f val
