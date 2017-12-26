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


firstUpdMuddy : Formula a
firstUpdMuddy =
    Disj
        [ Atom (redhat Ag.a)
        , Atom (redhat Ag.b)
        , Atom (redhat Ag.c)
        ]


nooneknows : Formula a
nooneknows =
    Neg someoneknows


someoneknows : Formula a
someoneknows =
    Disj [ aknows, bknows, cknows ]


everyoneknows : Formula a
everyoneknows =
    Conj [ aknows, bknows, cknows ]


aknows : Formula a
aknows =
    agentknows Ag.a


bknows : Formula a
bknows =
    agentknows Ag.b


cknows : Formula c
cknows =
    agentknows Ag.c


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


firstupdatetwo : Formula a
firstupdatetwo =
    Disj [ Atom <| redhat Ag.a, Atom <| redhat Ag.b ]


initupdatetwo : EpistM
initupdatetwo =
    upd_pa muddyChildrenTwo firstupdatetwo


afterfirstupdatetwo : EpistM
afterfirstupdatetwo =
    (upd_pa (upd_pa muddyChildrenTwo firstupdatetwo) nooneknows)


secondupdatetwo : EpistM
secondupdatetwo =
    (upd_pa (upd_pa (upd_pa muddyChildrenTwo firstupdatetwo) nooneknows) nooneknows)


thirdupdatetwo : EpistM
thirdupdatetwo =
    (upd_pa (upd_pa (upd_pa (upd_pa muddyChildrenTwo firstupdatetwo) nooneknows) nooneknows) nooneknows)
