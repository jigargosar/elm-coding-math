module E4CirclesEllipsesLissajousCurves exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (fill, r)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { angle : Float
    }


init : Model
init =
    { angle = 0 }


type Msg
    = Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg ->
            model


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
