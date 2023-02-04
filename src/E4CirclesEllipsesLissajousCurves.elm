module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, input, label, node, span, text)
import Html.Attributes exposing (step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Round
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, r)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { xSpeed : Float
    , ySpeed : Float
    , isPaused : Bool
    , elapsed : Float
    , elapsedBacking : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        elapsed =
            0
    in
    ( { xSpeed = turns 0.9745
      , ySpeed = turns 0.791
      , isPaused = True
      , elapsed = elapsed
      , elapsedBacking = String.fromFloat elapsed
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | TogglePause
    | XSpeedChanged String
    | YSpeedChanged String
    | ElapsedChanged String


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
            ( if model.isPaused then
                model

              else
                { model | elapsed = model.elapsed + ds }
            , Cmd.none
            )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )

        XSpeedChanged str ->
            case String.toFloat str of
                Just xSpeed ->
                    ( { model | xSpeed = xSpeed }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        YSpeedChanged str ->
            case String.toFloat str of
                Just ySpeed ->
                    ( { model | ySpeed = ySpeed }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ElapsedChanged str ->
            ( { model
                | elapsedBacking = str
                , isPaused = True
                , elapsed =
                    case String.toFloat str of
                        Just elapsed ->
                            elapsed

                        Nothing ->
                            model.elapsed
              }
            , Cmd.none
            )


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
        , viewConfigPanel model
        , viewSvg model
        ]


viewConfigPanel : Model -> Html Msg
viewConfigPanel model =
    div
        [ style "position" "fixed"
        , style "opacity" "0.8"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "10px"
            , style "padding" "10px"
            ]
            [ viewPauseButton model.isPaused
            , let
                elapsedString =
                    Round.round 2 model.elapsed ++ "s"
              in
              label []
                [ text "Elapsed: "
                , span
                    [ style "min-width" "10ch"
                    , style "display" "inline-block"
                    ]
                    [ text elapsedString ]
                ]
            , input
                [ type_ "range"
                , step "any"
                , Html.Attributes.min "0"
                , Html.Attributes.max "10"
                , value model.elapsedBacking
                , onInput ElapsedChanged
                ]
                []
            , viewFloatInput "xSpeed: " model.xSpeed XSpeedChanged
            , viewFloatInput "ySpeed: " model.ySpeed YSpeedChanged
            ]
        ]


fdivBy by x =
    x / by


roundUpto n f =
    f
        * (10 ^ n)
        |> round
        |> toFloat
        |> fdivBy (10 ^ n)


viewFloatInput labelText float msg =
    label []
        [ text labelText
        , input
            [ type_ "number"
            , value (String.fromFloat float)
            , onInput msg
            , step "0.01"
            ]
            []
        ]


viewPauseButton : Bool -> Html Msg
viewPauseButton isPaused =
    button
        [ onClick TogglePause
        , style "min-width" "10ch"
        ]
        [ let
            txt =
                case isPaused of
                    True ->
                        "Play"

                    False ->
                        "Pause"
          in
          text txt
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    svg
        [ style "background-color" "black"
        , style "width" "100%"
        , style "height" "100%"
        , fill "white"
        ]
        [ g [ style "transform" "translate(50%,50%)" ]
            [ let
                xRadius =
                    150

                yRadius =
                    200

                xAngle =
                    model.xSpeed * model.elapsed

                yAngle =
                    model.ySpeed * model.elapsed

                x =
                    cos xAngle * xRadius

                y =
                    sin yAngle * yRadius
              in
              circle
                [ cx (String.fromFloat x)
                , cy (String.fromFloat y)
                , r "1%"
                ]
                []
            ]
        ]


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
