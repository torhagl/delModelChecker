module ExampleModels exposing (..)

import DEMO_S5 as Demo exposing (EpistM)
import ValFunction as Val
import AccessRel as Acc
import Prop exposing (..)


exampleModel : EpistM
exampleModel =
    Demo.Mo [ 0, 1 ] [ "a", "b" ] (Acc.fromList [ ( "a", [ [ 0 ], [ 1 ] ] ), ( "b", [ [ 0, 1 ] ] ) ]) (Val.fromList [ ( 0, [ p, r, s, Pred "a" 0 ] ), ( 1, [ q ] ) ]) 0


russianCardsModel : EpistM
russianCardsModel =
    Demo.Mo (List.range 0 139) [ "a", "b", "c" ] Acc.empty Val.empty 0
