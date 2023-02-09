module LissajousCurveTable exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (style)
import Svg exposing (circle, g, line, polyline, svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , points
        , r
        , stroke
        , strokeOpacity
        , strokeWidth
        , viewBox
        , x1
        , x2
        , y1
        , y2
        )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- CONFIG


rows =
    4


cols =
    4


gridPoints =
    List.concatMap
        (\y ->
            List.map (\x -> ( x, y )) (List.range 0 cols)
        )
        (List.range 0 rows)
        |> List.drop 1


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
    turns 0.2


curvePointsToPreserve =
    -- this is a strange value, works differently on ellie and locally :?
    600



-- MODEL


type alias Model =
    { elapsed : Float
    , curves : Dict ( Int, Int ) (List ( Float, Float ))
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsed = 0, curves = initialCurves }, Cmd.none )


initialCurves : Dict ( Int, Int ) (List ( Float, Float ))
initialCurves =
    List.foldl (\gp -> Dict.insert gp (initCurve gp)) Dict.empty gridPoints
        |> Dict.filter (\( x, y ) _ -> x /= 0 && y /= 0)


initCurve : ( Int, Int ) -> List ( Float, Float )
initCurve ( xIdx, yIdx ) =
    [ curvePointAt 0 ( xIdx, yIdx ) ]


curvePointAt : Float -> ( Int, Int ) -> ( Float, Float )
curvePointAt elapsed ( xIdx, yIdx ) =
    let
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
    ( x, y )


updateCurves : Float -> Dict ( Int, Int ) (List ( Float, Float )) -> Dict ( Int, Int ) (List ( Float, Float ))
updateCurves elapsed =
    Dict.map (\gp curve -> curvePointAt elapsed gp :: curve |> List.take curvePointsToPreserve)


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
                    -- capping delta is so very important, and always
                    -- easy to forget
                    atMost (1000 / 60) millis / 1000

                elapsed =
                    model.elapsed + ds
            in
            ( { model | elapsed = elapsed, curves = updateCurves elapsed model.curves }, Cmd.none )


atMost =
    min


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
        , stroke white
        , strokeWidth "2"
        ]
        (List.map (viewCell model.elapsed) gridPoints
            ++ List.map viewCurve (Dict.toList model.curves)
        )


colorAt : ( Int, Int ) -> String
colorAt ( x, y ) =
    colorGrid
        |> listGetOr [] y
        |> listGetOr white x


listGetOr default i =
    List.drop i >> List.head >> Maybe.withDefault default



-- https://www.w3schools.com/colors/colors_groups.asp
-- https://www.w3schools.com/colors/colors_mixer.asp


colorGrid =
    [ [ white, yellow, green, blue, pink ]
    , [ yellow, yellow, yloGrn, bluYlo, pnkYlo ]
    , [ green, yloGrn, green, grnBlu, pnkGrn ]
    , [ blue, bluYlo, grnBlu, blue, pnkBlu ]
    , [ pink, pnkYlo, pnkGrn, pnkBlu, pink ]
    ]


white =
    "white"


yellow =
    "gold"


green =
    "limegreen"


blue =
    "dodgerblue"


pink =
    "deeppink"


yloGrn =
    "YellowGreen"


bluYlo =
    "#83b08c"


pnkYlo =
    "orange"


grnBlu =
    "#28ae98"


pnkGrn =
    "#987062"


pnkBlu =
    "#8e52c9"


viewCurve ( gp, pts ) =
    let
        ptToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        ptsString =
            List.map ptToString pts
                |> String.join " "
    in
    cellContainer gp
        [ polyline
            [ points ptsString
            , stroke (colorAt gp)
            ]
            []
        ]


viewCell elapsed ( xIdx, yIdx ) =
    let
        ( x, y ) =
            curvePointAt elapsed ( xIdx, yIdx )
    in
    cellContainer ( xIdx, yIdx )
        [ if xIdx == 0 || yIdx == 0 then
            g []
                [ circle
                    [ r (String.fromFloat cellRadius)
                    , stroke (colorAt ( xIdx, yIdx ))
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
        , viewDot x y
        ]


viewDot x y =
    circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r (String.fromFloat dotRadius)
        , stroke "none"
        , fill white
        ]
        []


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
body{
    margin:0;
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
