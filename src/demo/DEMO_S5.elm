module DEMO_S5 exposing (..)

import Expression exposing (..)
import Agent exposing (Agent)
import Prop exposing (Prop)
import ValFunction as Val exposing (ValFunction)
import AccessRel as Acc exposing (EqClass, AccessRel)
import State exposing (..)


type EpistM
    = Mo (List State) (List Agent) AccessRel ValFunction State


empty : EpistM
empty =
    Mo [] [] Acc.empty Val.empty 0


isTrueAt : EpistM -> State -> Expression a -> Bool
isTrueAt model st expression =
    let
        (Mo sts ags rel val _) =
            model
    in
        case expression of
            Top ->
                True

            Atom p ->
                List.member p <|
                    case Val.get st val of
                        Just a ->
                            a

                        Nothing ->
                            []

            Neg subf ->
                not <| isTrueAt model st subf

            Disj listOfDisj ->
                List.any (isTrueAt model st) listOfDisj

            Conj listOfConj ->
                List.all (isTrueAt model st) listOfConj

            Kn ag subexpr ->
                case Acc.getEqClass ag st rel of
                    Just eq ->
                        List.all (\s -> isTrueAt model s subexpr) eq

                    Nothing ->
                        False

            Pub upd subexpr ->
                False


upd_pa : EpistM -> Expression a -> EpistM
upd_pa model expr =
    let
        (Mo st ags rel val current) =
            model

        newsts =
            List.filter (\s -> isTrueAt model s expr) st

        newval =
            Val.filter (\k _ -> List.member k newsts) val

        newrel =
            Acc.map (restrict newsts) rel
    in
        (Mo newsts ags newrel newval current)


restrict : List State -> List EqClass -> List EqClass
restrict states eqclasses =
    List.filter (\y -> not <| y == []) <|
        List.map (List.filter (\s -> List.member s states)) eqclasses
