module Elm3d.Component exposing
    ( Component, Props, View
    , Model, init
    , Msg, update, subscriptions
    , view
    , toWindowSize, toUserModel
    )

{-|

@docs Component, Props, View
@docs Model, init
@docs Msg, update, subscriptions
@docs view

@docs toWindowSize, toUserModel

-}

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Asset
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Input
import Elm3d.Matrix4
import Elm3d.Node exposing (Node)
import FixedSet exposing (FixedSet)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Task
import WebGL


type Model model msg
    = Model
        { time : Float
        , window : ( Int, Int )
        , camera : Camera model msg
        , nodes : List (Node model msg)
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        , fps : FixedSet Float
        , model : model
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> View msg
        }


toUserModel : Model model msg -> model
toUserModel (Model model) =
    model.model


toWindowSize : Model model msg -> ( Int, Int )
toWindowSize (Model model) =
    model.window


type alias Component flags model msg =
    { init : Props model msg -> flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> View msg
    }


type alias Props model msg =
    { camera : Camera model msg
    , nodes : List (Node model msg)
    }


type alias View msg =
    { background : Color
    , hud : Html msg
    }


init : Props model msg -> Component flags model msg -> flags -> ( Model model msg, Cmd (Msg msg) )
init props component flags =
    let
        ( model, userCmd ) =
            component.init props flags

        { camera, nodes } =
            props

        objFileUrls : List String
        objFileUrls =
            List.concatMap Elm3d.Node.toObjFileUrls nodes

        ( assets, assetCmd ) =
            Elm3d.Asset.init { objFileUrls = objFileUrls }
    in
    ( Model
        { time = 0.0
        , window = ( 0, 0 )
        , camera = camera
        , nodes = nodes
        , input = Elm3d.Input.init
        , assets = assets
        , fps = FixedSet.init { maxSize = 30 }
        , model = model
        , update = component.update
        , subscriptions = component.subscriptions
        , view = component.view
        }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform Viewport
        , Cmd.map Asset assetCmd
        , Cmd.map User userCmd
        ]
    )



-- UPDATE


type Msg msg
    = Frame Float
    | Viewport Browser.Dom.Viewport
    | Resize Int Int
    | Input Elm3d.Input.RawEvent
    | Asset Elm3d.Asset.Msg
    | Focus Browser.Events.Visibility
    | ContextMenu
    | User msg


update : Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
update msg (Model model) =
    case msg of
        User msg_ ->
            -- TODO
            ( Model model
            , Cmd.none
            )

        ContextMenu ->
            ( Model model
            , Cmd.none
            )

        Asset assetsMsg ->
            let
                ( assets, assetsCmd ) =
                    -- TODO: Consider dynamic model.nodes here
                    -- to fetch objs not loaded at startup
                    Elm3d.Asset.update assetsMsg model.assets
            in
            ( Model { model | assets = assets }
            , Cmd.map Asset assetsCmd
            )

        Frame ms ->
            let
                dt : Float
                dt =
                    ms / 1000

                time : Float
                time =
                    model.time + dt

                ctx : Elm3d.Node.Context model
                ctx =
                    { dt = dt
                    , time = time
                    , input = model.input
                    , model = model.model
                    }

                ( camera, cmd ) =
                    Elm3d.Camera.update ctx model.camera

                nodesAndCmds =
                    List.map (Elm3d.Node.update ctx) model.nodes
            in
            ( Model
                { model
                    | time = time
                    , camera = camera
                    , nodes = List.map Tuple.first nodesAndCmds
                    , fps = FixedSet.insert dt model.fps
                }
            , Cmd.batch
                (cmd :: List.map Tuple.second nodesAndCmds)
                |> Cmd.map User
            )

        Viewport { scene } ->
            ( Model
                { model
                    | window =
                        ( Basics.floor scene.width
                        , Basics.floor scene.height
                        )
                }
            , Cmd.none
            )

        Resize width height ->
            ( Model { model | window = ( width, height ) }
            , Cmd.none
            )

        Focus _ ->
            ( Model { model | input = Elm3d.Input.releaseAllKeys model.input }
            , Cmd.none
            )

        Input internalEvent ->
            let
                ( input, maybeEvent ) =
                    Elm3d.Input.update
                        { event = internalEvent }
                        model.input
            in
            case maybeEvent of
                Just event ->
                    let
                        newNodesAndCmds =
                            List.map (Elm3d.Node.onInput event) model.nodes

                        ( newCamera, cameraCmd ) =
                            Elm3d.Camera.onInput event model.camera

                        newNodes =
                            List.map Tuple.first newNodesAndCmds
                    in
                    ( Model
                        { model
                            | input = input
                            , camera = newCamera
                            , nodes = newNodes
                        }
                    , Cmd.batch (cameraCmd :: List.map Tuple.second newNodesAndCmds)
                        |> Cmd.map User
                    )

                Nothing ->
                    ( Model { model | input = input }
                    , Cmd.none
                    )


subscriptions : Model model msg -> Sub (Msg msg)
subscriptions model =
    Sub.batch
        [ if hasAnyUpdateNodes model then
            Browser.Events.onAnimationFrameDelta Frame

          else
            Sub.none
        , Browser.Events.onResize Resize
        , Elm3d.Input.subscriptions Input
        , Browser.Events.onVisibilityChange Focus
        ]


hasAnyUpdateNodes : Model model msg -> Bool
hasAnyUpdateNodes (Model model) =
    Elm3d.Camera.hasUpdateFunction model.camera
        || List.any Elm3d.Node.hasUpdateFunction model.nodes


view : ( Int, Int ) -> Model model msg -> Html (Msg msg)
view size (Model model) =
    let
        { hud, background } =
            model.view model.model

        ( width, height ) =
            size

        directionalLight : Maybe (Node model msg)
        directionalLight =
            model.nodes
                |> List.Extra.find Elm3d.Node.isDirectionalLight

        viewCanvas : Html (Msg msg)
        viewCanvas =
            model.nodes
                |> List.concatMap
                    (Elm3d.Node.toEntity Elm3d.Matrix4.identity
                        { camera = Elm3d.Camera.toMatrix4 size model.camera
                        , light = directionalLight |> Maybe.map Elm3d.Node.toRotation
                        , assets = model.assets
                        }
                    )
                |> WebGL.toHtmlWith
                    [ WebGL.antialias
                    , WebGL.depth 1
                    , WebGL.alpha False
                    ]
                    [ Html.Attributes.width width
                    , Html.Attributes.height height
                    , Html.Attributes.style "background-color" (Elm3d.Color.toHtmlColor background)
                    , Html.Attributes.style "transform-origin" "top left"
                    ]

        viewFps : Html (Msg msg)
        viewFps =
            Html.div
                [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "1rem"
                , Html.Attributes.style "right" "1rem"
                , Html.Attributes.style "font-family" "sans-serif"
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "text-shadow" "0 -1px 1px black, 0 1px 1px black, -1px 0 1px black, 1px 0 1px black"
                ]
                [ case FixedSet.toAverage model.fps of
                    Just spf ->
                        Html.text
                            ((1 / spf)
                                |> Basics.round
                                |> String.fromInt
                                |> (\s -> s ++ " fps")
                            )

                    Nothing ->
                        Html.text ""
                ]
    in
    Html.div
        [ Html.Events.custom "contextmenu"
            (Json.Decode.succeed
                { message = ContextMenu
                , stopPropagation = True
                , preventDefault = True
                }
            )
        ]
        [ viewCanvas
        , hud |> Html.map User
        , viewFps
        ]
