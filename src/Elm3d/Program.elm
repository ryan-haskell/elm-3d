module Elm3d.Program exposing (Program, View, new)

import Browser
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Component exposing (Component, Model, Msg, Props)
import Elm3d.Node exposing (Node)
import Elm3d.Viewport exposing (Viewport)
import Html exposing (Html)
import Html.Attributes


type alias Program model msg =
    Platform.Program () (Model model msg) (Msg msg)


type alias View msg =
    { viewport : Viewport
    , background : Color
    , hud : Html msg
    }


new :
    { init : () -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , camera : Camera model msg
    , nodes : List (Node model msg)
    , view : model -> View msg
    }
    -> Program model msg
new program =
    let
        component : Component () model msg
        component =
            { init = \props () -> program.init ()
            , update = program.update
            , subscriptions = program.subscriptions
            , view =
                \model ->
                    let
                        { background, hud } =
                            program.view model
                    in
                    { background = background, hud = hud }
            }

        init : () -> ( Model model msg, Cmd (Msg msg) )
        init flags =
            Elm3d.Component.init
                { camera = program.camera
                , nodes = program.nodes
                }
                component
                flags
    in
    Browser.element
        { init = init
        , update = Elm3d.Component.update
        , subscriptions = Elm3d.Component.subscriptions
        , view =
            \bigModel ->
                let
                    { viewport } =
                        program.view (Elm3d.Component.toUserModel bigModel)
                in
                view viewport bigModel
        }


view : Viewport -> Model model msg -> Html (Msg msg)
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
