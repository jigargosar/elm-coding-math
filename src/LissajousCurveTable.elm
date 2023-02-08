module LissajousCurveTable exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (style)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)


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
            indices =
                List.range 1 4
         in
         List.map (viewCell model.elapsed) indices
        )


cellWidth =
    100


cellHeight =
    100


cellRadius =
    cellWidth * 0.45


viewCell elapsed xIdx =
    let
        x =
            toFloat xIdx * cellWidth + (cellWidth / 2)

        y =
            cellHeight / 2
    in
    circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r (String.fromFloat cellRadius)
        ]
        []


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
