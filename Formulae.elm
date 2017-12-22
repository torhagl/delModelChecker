module Formulae exposing (..)

import Agent exposing (..)
import Prop as P


type Formulae a
    = Top
    | Prp P.Prop
    | Ng (Formulae a)
    | Disj (List (Formulae a))


show : Formulae a -> String
show formulae =
    case formulae of
        Top ->
            "Top"

        Prp prop ->
            P.show prop

        Ng subForm ->
            "not(" ++ show subForm ++ ")"

        Disj listOfDisj ->
            List.foldl (++) "(" <| List.intersperse " or " <| List.map show listOfDisj
