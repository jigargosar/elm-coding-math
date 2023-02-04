module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (fill, r)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { angle : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { angle = 0 }, Cmd.none )


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
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
