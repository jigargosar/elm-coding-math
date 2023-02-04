module RandomLines exposing (main)

import Html exposing (div)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg exposing (Svg, g, line, svg)
import Svg.Attributes as A
    exposing
        ( fill
        , stroke
        , viewBox
        )


main =
    div
        [ style "border" "1px solid black"
        , style "display" "grid"
        , style "place-items" "center"
        , style "max-height" "100vh"
        , style "box-sizing" "border-box"
        ]
        [ let
            ( w, h ) =
                ( 300 / 2, 300 )
          in
          svg
            [ style "border" "1px solid black"
            , style "overflow" "visible"
            , style "box-sizing" "border-box"
            , style "max-height" "100vh"
            , A.width "100%"
            , viewBox
                ("0 0 "
                    ++ String.fromFloat w
                    ++ " "
                    ++ String.fromFloat h
                )

            --, width (String.fromFloat w)
            --, height (String.fromFloat h)
            , stroke "black"
            , fill "transparent"
            ]
            [ g [] (lines w h)
            ]
        ]


lines w h =
    Random.step
        (Random.list 100 (randomLine w h))
        (Random.initialSeed 0)
        |> Tuple.first


randomLine w h =
    Random.map4 line4
        (Random.float 0 w)
        (Random.float 0 h)
        (Random.float 0 w)
        (Random.float 0 h)


line4 x1 y1 x2 y2 =
    line
        [ A.x1 (String.fromFloat x1)
        , A.y1 (String.fromFloat y1)
        , A.x2 (String.fromFloat x2)
        , A.y2 (String.fromFloat y2)
        ]
        []
