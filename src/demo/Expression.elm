module Expression exposing (..)

import Agent exposing (..)
import Prop exposing (Prop)


type Expression a
    = Top
    | Atom Prop
    | Neg (Expression a)
    | Disj (List (Expression a))
    | Conj (List (Expression a))
    | Kn Agent (Expression a)
    | Pub (Expression a) (Expression a)


show : Expression a -> String
show expression =
    case expression of
        Top ->
            "Top"

        Atom prop ->
            Prop.show prop

        Neg subexpr ->
            "not(" ++ show subexpr ++ ")"

        Disj listOfDisj ->
            (List.foldl (++) "(" <| List.intersperse " or " <| List.map show listOfDisj) ++ ")"

        Conj listOfConj ->
            (List.foldl (++) "(" <| List.intersperse " and " <| List.map show listOfConj) ++ ")"

        Kn ag subexpr ->
            "(Kn" ++ ag ++ show subexpr ++ ")"

        Pub upd subexpr ->
            "([" ++ show upd ++ "]" ++ show subexpr ++ ")"
