module Elm3d.Program exposing (Program, new)

import Browser
import Elm3d.Camera
import Elm3d.Color exposing (Color)
import Elm3d.Component exposing (Model, Msg)
import Elm3d.Node exposing (Node)
import Elm3d.Viewport exposing (Viewport)
import Html exposing (Html)
import Html.Attributes


type alias Program =
    Platform.Program () Model Msg


new :
    { background : Color
    , viewport : Viewport
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }
    -> Program
new props =
    Browser.element
        { init =
            Elm3d.Component.init
                { background = props.background
                , camera = props.camera
                , nodes = props.nodes
                }
        , update = Elm3d.Component.update
        , subscriptions = Elm3d.Component.subscriptions
        , view = view props.viewport
        }


view : Viewport -> Model -> Html Msg
view viewport model =
    let
        size : ( Int, Int )
        size =
            Elm3d.Viewport.toSize
                (Elm3d.Component.toWindowSize model)
                viewport
    in
    if Elm3d.Viewport.isFullscreen viewport then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; background: black; }""" ]
            , Elm3d.Component.view size model
            ]

    else if Elm3d.Viewport.isFullscreenAspect viewport then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; height: 100%; background: black; } .elm-3d { height: 100%; display: flex; align-items: center; justify-content: center; }""" ]
            , Elm3d.Component.view size model
            ]

    else
        Elm3d.Component.view size model
