module Prop exposing (..)

import Agent exposing (..)


type Prop
    = P Int
    | Q Int
    | R Int
    | S Int
    | Pred Agent Int -- Indicate that an agent has a number. Useful in situations where you are modeling agents holding cards or similar cases.


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
            if int == 0 then
                "p"
            else
                "p" ++ toString int

        Q int ->
            if int == 0 then
                "q"
            else
                "q" ++ toString int

        R int ->
            if int == 0 then
                "r"
            else
                "r" ++ toString int

        S int ->
            if int == 0 then
                "s"
            else
                "s" ++ toString int

        Pred ag int ->
            toString int ++ ag
