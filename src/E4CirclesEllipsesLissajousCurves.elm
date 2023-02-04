module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (circle, foreignObject, g, svg)
import Svg.Attributes exposing (cx, cy, fill, r)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { xAngle : Float
    , yAngle : Float
    , xSpeed : Float
    , ySpeed : Float
    , isPaused : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { xAngle = 0
      , yAngle = 0
      , xSpeed = turns 0.9745
      , ySpeed = turns 0.791
      , isPaused = True
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | Toggle


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
                { model
                    | xAngle = model.xAngle + (model.xSpeed * ds)
                    , yAngle = model.yAngle + (model.ySpeed * ds)
                }
            , Cmd.none
            )

        Toggle ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "grid"
        , style "width" "100vw"
        , style "height" "100vh"
        ]
        [ div [ style "position" "fixed" ]
            [ button [ onClick Toggle ] [ text "toggle" ] ]
        , svg
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

                    x =
                        cos model.xAngle * xRadius

                    y =
                        sin model.yAngle * yRadius
                  in
                  circle
                    [ cx (String.fromFloat x)
                    , cy (String.fromFloat y)
                    , r "1%"
                    ]
                    []
                ]
            ]
        ]
