module Formula exposing (..)

import Agent exposing (..)
import Prop exposing (Prop)


type Formula a
    = Top
    | Atom Prop
    | Neg (Formula a)
    | Disj (List (Formula a))
    | Conj (List (Formula a))
    | Kn Agent (Formula a)
    | Pub (Formula a) (Formula a)


show : Formula a -> String
show formula =
    case formula of
        Top ->
            "Top"

        Atom prop ->
            Prop.show prop

        Neg subform ->
            "not(" ++ show subform ++ ")"

        Disj listOfDisj ->
            (List.foldl (++) "(" <| List.intersperse " or " <| List.map show listOfDisj) ++ ")"

        Conj listOfConj ->
            (List.foldl (++) "(" <| List.intersperse " and " <| List.map show listOfConj) ++ ")"

        Kn ag subform ->
            "(Kn" ++ ag ++ show subform ++ ")"

        Pub upd subform ->
            "([" ++ show upd ++ "]" ++ show subform ++ ")"
