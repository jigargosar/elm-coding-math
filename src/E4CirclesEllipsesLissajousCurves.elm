module E4CirclesEllipsesLissajousCurves exposing (main)

import Html.Attributes exposing (style)
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (fill, r)


main =
    svg
        [ style "background-color" "black"
        , style "display" "flex"
        , style "width" "100vw"
        , style "height" "100vh"
        , fill "white"
        ]
        [ g [ style "transform" "translate(50%,50%)" ]
            [ circle [ r "100" ] []
            ]
        ]
