module DEMO_S5 exposing (..)

import Formula exposing (..)
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


isTrueAt : EpistM -> State -> Formula a -> Bool
isTrueAt model st formula =
    let
        (Mo sts ags rel val _) =
            model
    in
        case formula of
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

            Kn ag subform ->
                case Acc.getEqClass ag st rel of
                    Just eq ->
                        List.all (\s -> isTrueAt model s subform) eq

                    Nothing ->
                        False

            Pub upd subform ->
                let
                    newmodel =
                        (upd_pa model upd)

                    (Mo newsts _ _ _ _) =
                        newmodel
                in
                    -- Current state must exist in the new model, otherwise announcement was false and formula therefore satisfied.
                    if List.member st newsts then
                        isTrueAt newmodel st subform
                    else
                        True



{-
   Do a public announcement update.
   States that carry over are the ones where the formula is true.
   New accessability relation is done using restrict on the equivalence classes of every agent.
-}


upd_pa : EpistM -> Formula a -> EpistM
upd_pa model formula =
    let
        (Mo st ags rel val current) =
            model

        newsts =
            List.filter (\s -> isTrueAt model s formula) st

        newrel =
            Acc.mapWithoutKey (restrict newsts) rel
    in
        (Mo newsts ags newrel val current)



-- First checks all equivalence classes whether each state is a member of the set of states in the new model, if not they are removed from the equivalence class.
-- Then apply a filter to remove all equivalence classes that had all its members removed.


restrict : List State -> List EqClass -> List EqClass
restrict states eqclasses =
    List.filter
        (\y -> not <| y == [])
    <|
        List.map
            (List.filter
                (\s -> List.member s states)
            )
            eqclasses
