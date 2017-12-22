module ValFunction exposing (..)

import State exposing (..)
import Dict exposing (..)
import Prop exposing (..)


type alias ValFunction =
    Maybe (Dict State (List Prop))


empty : ValFunction
empty =
    Nothing


get : State -> ValFunction -> Maybe (List Prop)
get st v =
    case v of
        Just dict ->
            case Dict.get st dict of
                Just a ->
                    Just a

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
