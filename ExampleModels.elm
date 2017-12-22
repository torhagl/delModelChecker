module ExampleModels exposing (..)

import DEMO_S5 as Demo exposing (EpistM)
import Dict
import Prop


exampleModel : EpistM
exampleModel =
    Demo.Mo [ 0, 1 ] [ "a", "b" ] (Dict.fromList [ ( "a", [ [ 0 ], [ 1 ] ] ), ( "b", [ [ 0, 1 ] ] ) ]) (Dict.fromList [ ( 0, [ Prop.p, Prop.r, Prop.s ] ), ( 1, [ Prop.q ] ) ]) 1
