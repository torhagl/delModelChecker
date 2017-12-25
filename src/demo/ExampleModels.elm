module ExampleModels exposing (..)

import DEMO_S5 as Demo exposing (EpistM)
import ValFunction as Val exposing (ValFunction)
import AccessRel as Acc exposing (AccessRel, EqClass)
import Prop exposing (..)
import Agent as Ag


exampleModel : EpistM
exampleModel =
    Demo.Mo [ 0, 1 ] [ Ag.a, Ag.b ] (Acc.fromList [ ( Ag.a, [ [ 0 ], [ 1 ] ] ), ( Ag.b, [ [ 0, 1 ] ] ) ]) (Val.fromList [ ( 0, [ p, r, s, Pred Ag.a 0 ] ), ( 1, [ q ] ) ]) 0


muddyChildren : EpistM
muddyChildren =
    Demo.Mo (List.range 0 7) [ Ag.a, Ag.b, Ag.c ] muddyAcc muddyVal 0



-- Accessability relation, each agent has two states in each because they can't see their own hat, but they can see both the others. Therefore the states considered equal are the ones where the two other agents have the same hats.


muddyAcc : AccessRel
muddyAcc =
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


redhata : Prop
redhata =
    P 0


redhatb : Prop
redhatb =
    P 1


redhatc : Prop
redhatc =
    P 2


muddyVal : ValFunction
muddyVal =
    Val.fromList
        [ ( 0, [ redhata, redhatb, redhatc ] )
        , ( 1, [ redhata, redhatb ] )
        , ( 2, [ redhata, redhatc ] )
        , ( 3, [ redhata ] )
        , ( 4, [ redhatb, redhatc ] )
        , ( 5, [ redhatb ] )
        , ( 6, [ redhatc ] )
        , ( 7, [] )
        ]
