module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (style)
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
    { xAngle : Float
    , yAngle : Float
    , xSpeed : Float
    , ySpeed : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { xAngle = 0
      , yAngle = 0
      , xSpeed = turns 0.9745
      , ySpeed = turns 0.791
      }
    , Cmd.none
    )


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
            ( { model
                | xAngle = model.xAngle + (model.xSpeed * ds)
                , yAngle = model.yAngle + (model.ySpeed * ds)
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    svg
        [ style "background-color" "black"
        , style "display" "flex"
        , style "width" "100vw"
        , style "height" "100vh"
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
                , r "10"
                ]
                []
            ]
        ]
