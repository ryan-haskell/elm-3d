module Embedded exposing (main)

import Browser
import Elm3d.Camera exposing (Camera)
import Elm3d.Color
import Elm3d.Component
import Elm3d.Context exposing (Context)
import Elm3d.Node exposing (Node)
import Elm3d.Vector3
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { elm3d : Elm3d.Component.Model
    , angle : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { angle = 0
            }

        ( elm3d, elmCmd ) =
            Elm3d.Component.init
                { nodes = nodes model
                }
    in
    ( { elm3d = elm3d
      , angle = model.angle
      }
    , Cmd.map Elm3d elmCmd
    )



-- UPDATE


type Msg
    = Elm3d Elm3d.Component.Msg
    | Spin Context


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spin ctx ->
            ( { model | angle = model.angle + ctx.dt }
            , Cmd.none
            )

        Elm3d elm3dMsg ->
            Elm3d.Component.update
                { toMsg = Elm3d
                , toModel = \elm3d -> { model | elm3d = elm3d }
                , model = model.elm3d
                , msg = elm3dMsg
                , camera = camera
                , nodes = nodes model
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    model.elm3d
        |> Elm3d.Component.subscriptions
            { fpsLimit = Nothing
            , nodes = nodes model
            , camera = camera
            }
        |> Sub.map Elm3d



-- VIEW


nodes : { model | angle : Float } -> List (Node Msg)
nodes { angle } =
    [ Elm3d.Node.cube
        { size = 1
        , color = Elm3d.Color.magenta
        }
        |> Elm3d.Node.withRotationX (pi / 6)
        |> Elm3d.Node.withRotationY angle
        |> Elm3d.Node.withOnFrame Spin
    ]


camera : Camera Msg
camera =
    Elm3d.Camera.orthographic
        { size = 2
        , range = ( 1, 1000 )
        }
        |> Elm3d.Camera.withPositionZ 500


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.5rem"
        , style "font-family" "sans-serif"
        , style "padding-bottom" "1rem"
        ]
        [ node "style" [] [ text """
            html, body { height: 100%; margin: 0; }
            body { display: flex; align-items: center; justify-content: center; background: #121212; color: white; }
        """ ]
        , Elm3d.Component.view
            { showFps = True
            , size = ( 64, 64 )
            , toMsg = Elm3d
            , background = Elm3d.Color.transparent
            , nodes = nodes model
            , camera = camera
            }
            model.elm3d
        , h1 [ style "margin" "0" ] [ text "Hello, Elm3D!" ]
        ]
