module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, input, label, node, span, text)
import Html.Attributes exposing (step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Round
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, r, transform, viewBox)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { obj : Object
    , objects : List Object
    , xRadius : Float
    , yRadius : Float
    , isPaused : Bool
    , elapsed : Float
    , elapsedBacking : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        elapsed =
            0

        objects : List Object
        objects =
            Random.step (Random.list 20 randomObject) (Random.initialSeed 0)
                |> Tuple.first
    in
    ( { obj =
            { xSpeed = turns 0.5745
            , ySpeed = turns 0.691
            }
      , objects = objects
      , xRadius = 200
      , yRadius = 250
      , isPaused = False
      , elapsed = elapsed
      , elapsedBacking = String.fromFloat elapsed
      }
    , Cmd.none
    )


type alias Object =
    { xSpeed : Float
    , ySpeed : Float
    }


randomObject =
    Random.map2 Object
        randomSpeed
        randomSpeed


randomSpeed =
    Random.float 0.2 0.6
        |> Random.map turns


type Msg
    = Tick Float
    | TogglePause
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

        --|> always (text "")
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
            ]
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
        [ viewBox "-300 -300 600 600"
        , style "background-color" "#111"
        , style "max-width" "100%"
        , style "max-height" "100%"
        , fill "white"
        ]
        (viewObject model model.obj
            :: List.map (viewObject model) model.objects
        )


viewObject : Model -> Object -> Svg.Svg msg
viewObject model obj =
    let
        xAngle =
            obj.xSpeed * model.elapsed

        yAngle =
            obj.ySpeed * model.elapsed

        x =
            cos xAngle * model.xRadius

        y =
            sin yAngle * model.yRadius
    in
    circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r "3"
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
