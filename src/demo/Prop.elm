module Prop exposing (..)

import Agent exposing (..)


type Prop
    = P Int
    | Pred Agent Int -- Indicate that something is true for an agent, with an int for multiple propositions on same agent.


p : Prop
p =
    P 0


q : Prop
q =
    P 1


r : Prop
r =
    P 2


s : Prop
s =
    P 3


getAgent : Prop -> Maybe Agent
getAgent prop =
    case prop of
        P _ ->
            Nothing

        Pred ag _ ->
            Just ag


show : Prop -> String
show prop =
    case prop of
        P int ->
            if int == 0 then
                "p"
            else
                "p" ++ toString int

        Pred ag int ->
            toString int ++ ag
