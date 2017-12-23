module ValFunction exposing (..)

import State exposing (..)
import Dict exposing (Dict)
import Prop exposing (..)


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
    case list of
        [] ->
            empty

        ( st, props ) :: xs ->
            insert st props <| fromList xs
