module Prop exposing (..)

import Agent exposing (..)


type Prop
    = P Int
    | Q Int
    | R Int
    | S Int
    | Pred Agent Int -- Works as a predicate with closed world assumption. Handy in some situations.


p : Prop
p =
    P 0


q : Prop
q =
    Q 0


r : Prop
r =
    R 0


s : Prop
s =
    S 0


show : Prop -> String
show prop =
    case prop of
        P int ->
            "P" ++ toString int

        Q int ->
            "Q" ++ toString int

        R int ->
            "R" ++ toString int

        S int ->
            "S" ++ toString int

        Pred ag int ->
            toString int ++ ag
