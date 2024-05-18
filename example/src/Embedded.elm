module Embedded exposing (main)

import Browser
import Elm3d.Camera exposing (Camera)
import Elm3d.Color
import Elm3d.Component
import Elm3d.Node exposing (Context, Node)
import Elm3d.Texture
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
        ( elm3d, elmCmd ) =
            Elm3d.Component.init
                { nodes = nodes model
                }

        model =
            { angle = 0
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
    | Spin Context (Node Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spin ctx node ->
            ( { model | angle = model.angle + ctx.dt * 0.5 }
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
            { fpsLimit = Just 30
            , nodes = nodes model
            , camera = camera
            }
        |> Sub.map Elm3d



-- VIEW


nodes : { model | angle : Float } -> List (Node Msg)
nodes { angle } =
    [ Elm3d.Node.cube
        { size = 1
        , texture = Elm3d.Texture.color Elm3d.Color.magenta
        }
        |> Elm3d.Node.withRotationX (pi / 6)
        |> Elm3d.Node.withRotationY angle
        |> Elm3d.Node.withOnUpdate Spin
    ]


camera : Camera Msg
camera =
    Elm3d.Camera.orthographic
        { size = 2
        , near = 1
        , far = 1000
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
            { showFps = False
            , size = ( 64, 64 )
            , toMsg = Elm3d
            , background = Elm3d.Color.transparent
            , nodes = nodes model
            , camera = camera
            }
            model.elm3d
        , h1 [ style "margin" "0" ] [ text "Hello, Elm3D!" ]
        ]
