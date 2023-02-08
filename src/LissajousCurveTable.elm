module LissajousCurveTable exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (style)
import Svg exposing (circle, g, line, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeOpacity, strokeWidth, transform, viewBox, x1, x2, y1, y2)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { elapsed : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsed = 0 }, Cmd.none )


type Msg
    = Tick Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick millis ->
            let
                ds =
                    millis / 1000
            in
            ( { model | elapsed = model.elapsed + ds }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "grid"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "font-family" "monospace"
        , style "font-size" "1rem"
        ]
        [ globalStyles
        , viewSvg model
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    svg
        [ viewBox "0 0 500 500"
        , style "background-color" "#111"
        , style "max-width" "100%"
        , style "max-height" "100%"
        , fill "none"
        , stroke "white"
        , strokeWidth "2"
        ]
        (let
            xIndices =
                List.range 0 cols

            yIndices =
                List.range 0 rows

            gridPoints =
                List.concatMap
                    (\y ->
                        List.map (\x -> ( x, y )) xIndices
                    )
                    yIndices
         in
         List.map (viewCell model.elapsed) gridPoints
        )


rows =
    4


cols =
    4


cellWidth =
    100


cellHeight =
    100


gridWidth =
    (cols + 1) * cellWidth


gridHeight =
    (rows + 1) * cellWidth


cellRadius =
    cellWidth * 0.45


dotRadius =
    cellRadius * 0.1


baseSpeed =
    turns 0.1


viewCell elapsed ( xIdx, yIdx ) =
    if xIdx == 0 && yIdx == 0 then
        text ""

    else
        let
            centerX =
                toFloat xIdx * cellWidth + (cellWidth / 2)

            centerY =
                toFloat yIdx * cellHeight + (cellHeight / 2)

            xSpeed =
                baseSpeed
                    * toFloat
                        (if xIdx == 0 then
                            yIdx

                         else
                            xIdx
                        )

            xAngle =
                turns -0.25 + (xSpeed * elapsed)

            ySpeed =
                baseSpeed
                    * toFloat
                        (if yIdx == 0 then
                            xIdx

                         else
                            yIdx
                        )

            yAngle =
                turns -0.25 + (ySpeed * elapsed)

            x =
                cellRadius * cos xAngle

            y =
                cellRadius * sin yAngle
        in
        cellContainer ( xIdx, yIdx )
            [ if xIdx == 0 || yIdx == 0 then
                g []
                    [ circle
                        [ r (String.fromFloat cellRadius)
                        ]
                        []
                    , if xIdx == 0 then
                        line
                            [ x1 (String.fromFloat x)
                            , y1 (String.fromFloat y)
                            , x2 (String.fromFloat gridWidth)
                            , y2 (String.fromFloat y)
                            , strokeWidth "1"
                            , strokeOpacity "0.2"
                            ]
                            []

                      else
                        line
                            [ x1 (String.fromFloat x)
                            , y1 (String.fromFloat y)
                            , x2 (String.fromFloat x)
                            , y2 (String.fromFloat gridHeight)
                            , strokeWidth "1"
                            , strokeOpacity "0.2"
                            ]
                            []
                    ]

              else
                text ""
            , circle
                [ cx (String.fromFloat x)
                , cy (String.fromFloat y)
                , r (String.fromFloat dotRadius)
                , stroke "none"
                , fill "white"
                ]
                []
            ]


cellContainer ( xIdx, yIdx ) =
    let
        centerX =
            toFloat xIdx * cellWidth + (cellWidth / 2)

        centerY =
            toFloat yIdx * cellHeight + (cellHeight / 2)
    in
    g [ svgTransforms [ svgTranslateXY centerX centerY ] ]


svgTransforms list =
    Svg.Attributes.transform (String.join " " list)


svgTranslateXY x y =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


globalStyles =
    node "style" [] [ text """
:root {
    font-family: monospace;
    font-size: 16px;
    background-color: black;
    color: white;
}
* {
    box-sizing: border-box;
}
input, button {
    font-family: inherit;
    font-size: 100%;
    background-color: #222;
    color: inherit;
    border: none;
    padding: 0.5rem;
}
""" ]
