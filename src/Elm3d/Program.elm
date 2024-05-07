module Elm3d.Program exposing (Program, new)

import Browser
import Elm3d.Camera
import Elm3d.Component exposing (Model, Msg)
import Elm3d.Node exposing (Node)
import Elm3d.Window exposing (Window)
import Html exposing (Html)
import Html.Attributes


type alias Program =
    Platform.Program () Model Msg


new :
    { window : Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }
    -> Program
new props =
    Browser.element
        { init =
            Elm3d.Component.init
                { camera = props.camera
                , nodes = props.nodes
                }
        , update = Elm3d.Component.update
        , subscriptions = Elm3d.Component.subscriptions
        , view = view props
        }


view :
    { window : Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }
    -> Model
    -> Html Msg
view { window, camera, nodes } model =
    let
        size : ( Int, Int )
        size =
            Elm3d.Window.toSize (Elm3d.Component.toViewportSize model) window

        props : Elm3d.Component.Props
        props =
            { camera = camera
            , nodes = nodes
            }
    in
    if Elm3d.Window.isFullscreen window then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """body { margin: 0; overflow: hidden; }""" ]
            , Elm3d.Component.view size props model
            ]

    else if Elm3d.Window.isFullscreenAspect window then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; height: 100%; } .elm-3d { height: 100%; display: flex; align-items: center; justify-content: center; }""" ]
            , Elm3d.Component.view size props model
            ]

    else
        Elm3d.Component.view size props model
