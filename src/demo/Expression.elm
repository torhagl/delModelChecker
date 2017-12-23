module Expression exposing (..)

import Agent exposing (..)
import Prop exposing (Prop)


type Expression a
    = Top
    | Atom Prop
    | Neg (Expression a)
    | Disj (List (Expression a))
    | Conj (List (Expression a))


show : Expression a -> String
show expression =
    case expression of
        Top ->
            "Top"

        Atom prop ->
            Prop.show prop

        Neg subForm ->
            "not(" ++ show subForm ++ ")"

        Disj listOfDisj ->
            (List.foldl (++) "(" <| List.intersperse " or " <| List.map show listOfDisj) ++ ")"

        Conj listOfConj ->
            (List.foldl (++) "(" <| List.intersperse " and " <| List.map show listOfConj) ++ ")"
