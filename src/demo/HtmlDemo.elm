module HtmlDemo exposing (..)

import DEMO_S5 exposing (..)
import State exposing (State)
import Agent exposing (Agent)
import AccessRel as Acc exposing (EqClass, AccessRel)
import ValFunction as Val exposing (ValFunction)
import Html exposing (Html)
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Svg exposing (..)
import Svg.Attributes exposing (..)


htmlModel : EpistM -> Html msg
htmlModel model =
    let
        (Mo sts ags acc val current) =
            model
    in
        Grid.row []
            [ Grid.col [] [ htmlStates sts ]
            ]


htmlStates : List State -> Html msg
htmlStates sts =
    Card.config [ Card.outlinePrimary ]
        |> Card.headerH5 [] [ Html.text "States" ]
        |> Card.block []
            [ Card.titleH6 [] [ Html.text "Title" ] ]
        |> Card.view


stateNode : String -> StatePlacement -> List (Svg msg)
stateNode radius ( state, coord ) =
    [ circle
        [ cx <| toString coord.x
        , cy <| toString coord.y
        , r radius
        , fill "peachpuff"
        ]
        []
    , text_
        [ x <| toString (coord.x - 5)
        , y <| toString (coord.y + 5)
        , fill "white"
        ]
        [ text <| toString state ]
    ]


labeledEdge : EdgePlacement -> List (Svg msg)
labeledEdge ( agent, ( coord1, coord2 ) ) =
    [ line
        [ x1 <| toString coord1.x
        , y1 <| toString coord1.y
        , x2 <| toString coord2.x
        , y2 <| toString coord2.y
        , stroke "black"
        , strokeWidth "1"
        ]
        []
    , text_
        [ x <| toString ((coord1.x + coord2.x) // 2)
        , y <| toString ((coord1.y + coord2.y) // 2)
        , fill "black"
        ]
        [ text agent ]
    ]


type alias EdgePlacement =
    ( Agent, ( Coordinate, Coordinate ) )


type alias StatePlacement =
    ( State, Coordinate )


type alias Coordinate =
    { x : Int, y : Int }


muddyTwoModel : Html msg
muddyTwoModel =
    svg
        [ width "200"
        , height "200"
        , viewBox "0 0 200 200"
        ]
    <|
        List.concat <|
            (List.map labeledEdge <|
                eqClassEdges "a" [ 0, 2, 3 ] muddyTwoStatePlacement
            )
                ++ List.map (stateNode "20") muddyTwoStatePlacement


svgEdges : AccessRel -> List EdgePlacement
svgEdges acc =
    []


eqClassEdges : Agent -> EqClass -> List StatePlacement -> List EdgePlacement
eqClassEdges agent eq sts =
    let
        actualStates =
            List.filter (\( s, _ ) -> List.member s eq) sts
    in
        edgePlacement agent (lengthTwoSubsets eq) actualStates


lengthTwoSubsets : List a -> List ( a, a )
lengthTwoSubsets list =
    case list of
        [] ->
            []

        [ a ] ->
            []

        h :: tl ->
            makePairsWithRest h tl ++ lengthTwoSubsets tl


makePairsWithRest : a -> List b -> List ( a, b )
makePairsWithRest hd list =
    case list of
        [] ->
            []

        h :: tl ->
            ( hd, h ) :: makePairsWithRest hd tl


edgePlacement : Agent -> List ( State, State ) -> List StatePlacement -> List EdgePlacement
edgePlacement ag points sts =
    case points of
        [] ->
            []

        ( s1, s2 ) :: tl ->
            let
                firstState =
                    case List.head <| List.filter (\( s, _ ) -> s == s1) sts of
                        Just a ->
                            a

                        Nothing ->
                            ( 1337, { x = 0, y = 0 } )

                secondState =
                    case List.head <| List.filter (\( s, _ ) -> s == s2) sts of
                        Just a ->
                            a

                        Nothing ->
                            ( 1337, { x = 0, y = 0 } )
            in
                oneEdge ag ( firstState, secondState ) :: edgePlacement ag tl sts


oneEdge : Agent -> ( StatePlacement, StatePlacement ) -> EdgePlacement
oneEdge ag ( ( _, coord1 ), ( _, coord2 ) ) =
    ( ag, ( coord1, coord2 ) )


muddyTwoStatePlacement : List StatePlacement
muddyTwoStatePlacement =
    [ ( 0, { x = 20, y = 20 } )
    , ( 1, { x = 180, y = 20 } )
    , ( 2, { x = 20, y = 180 } )
    , ( 3, { x = 180, y = 180 } )
    ]


rectangleStates : String
rectangleStates =
    "a"



--, text_ [ x "15", y "25", fill "white" ] [ text <| toString agent ] ]
