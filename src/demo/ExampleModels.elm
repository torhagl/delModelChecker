module ExampleModels exposing (..)

import DEMO_S5 as Demo exposing (EpistM, upd_pa)
import ValFunction as Val exposing (ValFunction)
import AccessRel as Acc exposing (AccessRel, EqClass)
import Prop exposing (..)
import Formula as Form exposing (..)
import Agent as Ag exposing (Agent)


exampleModel : EpistM
exampleModel =
    Demo.Mo [ 0, 1 ] [ Ag.a, Ag.b ] (Acc.fromList [ ( Ag.a, [ [ 0 ], [ 1 ] ] ), ( Ag.b, [ [ 0, 1 ] ] ) ]) (Val.fromList [ ( 0, [ p, r, s, Pred Ag.a 0 ] ), ( 1, [ q ] ) ]) 0


muddyChildren : EpistM
muddyChildren =
    Demo.Mo (List.range 0 7) [ Ag.a, Ag.b, Ag.c ] muddyAccThree muddyValThree 0


muddyChildrenTwo : EpistM
muddyChildrenTwo =
    Demo.Mo (List.range 0 3) [ Ag.a, Ag.b ] muddyAccTwo muddyValTwo 0



-- Accessability relation, each agent has two states in each because they can't see their own hat, but they can see both the others. Therefore the states considered equal are the ones where the two other agents have the same hats.


muddyAccThree : AccessRel
muddyAccThree =
    Acc.fromList
        [ ( Ag.a
          , [ [ 0, 4 ]
            , [ 1, 5 ]
            , [ 2, 6 ]
            , [ 3, 7 ]
            ]
          )
        , ( Ag.b
          , [ [ 0, 2 ]
            , [ 1, 3 ]
            , [ 4, 6 ]
            , [ 5, 7 ]
            ]
          )
        , ( Ag.c
          , [ [ 0, 1 ]
            , [ 2, 3 ]
            , [ 4, 5 ]
            , [ 6, 7 ]
            ]
          )
        ]


muddyAccTwo : AccessRel
muddyAccTwo =
    Acc.fromList
        [ ( Ag.a
          , [ [ 0, 2 ]
            , [ 1, 3 ]
            ]
          )
        , ( Ag.b
          , [ [ 0, 1 ]
            , [ 2, 3 ]
            ]
          )
        ]


muddyValTwo : ValFunction
muddyValTwo =
    Val.fromList
        [ ( 0, [ redhat Ag.a, redhat Ag.b ] )
        , ( 1, [ redhat Ag.a ] )
        , ( 2, [ redhat Ag.b ] )
        , ( 3, [] )
        ]


nooneknows : List Agent -> Formula a
nooneknows agents =
    Conj <| List.map (agentdoesntknow) agents


everyoneknows : List Agent -> Formula a
everyoneknows agents =
    Conj <| List.map agentknows agents


agentdoesntknow : Agent -> Formula a
agentdoesntknow ag =
    Neg <| agentknows ag


agentknows : Agent -> Formula a
agentknows ag =
    Disj [ Kn ag <| Atom (redhat ag), Kn ag <| Neg <| Atom (redhat ag) ]


redhat : Agent -> Prop
redhat ag =
    Pred ag 0


muddyValThree : ValFunction
muddyValThree =
    Val.fromList
        [ ( 0, [ redhat Ag.a, redhat Ag.b, redhat Ag.c ] )
        , ( 1, [ redhat Ag.a, redhat Ag.b ] )
        , ( 2, [ redhat Ag.a, redhat Ag.c ] )
        , ( 3, [ redhat Ag.a ] )
        , ( 4, [ redhat Ag.b, redhat Ag.c ] )
        , ( 5, [ redhat Ag.b ] )
        , ( 6, [ redhat Ag.c ] )
        , ( 7, [] )
        ]


atleastoneredhat : List Agent -> Formula a
atleastoneredhat agents =
    Disj <| List.map (\ag -> Atom <| redhat ag) agents


updateMuddymodelNTimes : EpistM -> Int -> EpistM
updateMuddymodelNTimes epistm n =
    let
        (Demo.Mo _ ags _ _ _) =
            epistm
    in
        case n of
            0 ->
                epistm

            1 ->
                upd_pa epistm <| atleastoneredhat ags

            n ->
                upd_pa (updateMuddymodelNTimes (upd_pa epistm (nooneknows ags)) (n - 1)) <| nooneknows ags
