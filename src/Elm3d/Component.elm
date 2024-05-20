module Elm3d.Component exposing
    ( Model, init
    , Msg, update, subscriptions
    , View, view
    , toWindowSize
    )

{-|

@docs Model, init
@docs Msg, update, subscriptions
@docs View, view

@docs toWindowSize

-}

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Asset
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Context exposing (Context)
import Elm3d.Input
import Elm3d.Input.Event exposing (Event)
import Elm3d.Matrix4
import Elm3d.Node exposing (Node)
import FixedSet exposing (FixedSet)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Task
import Time
import WebGL


type Model
    = Model
        { time : Float
        , window : ( Int, Int )
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        , fps : FixedSet Float
        }


toWindowSize : Model -> ( Int, Int )
toWindowSize (Model model) =
    model.window


type alias View msg =
    { background : Color
    , camera : Camera msg
    , nodes : List (Node msg)
    }


init :
    { nodes : List (Node msg)
    }
    -> ( Model, Cmd Msg )
init { nodes } =
    let
        objFileUrls : List String
        objFileUrls =
            List.concatMap Elm3d.Node.toObjFileUrls nodes

        ( assets, assetCmd ) =
            Elm3d.Asset.init { objFileUrls = objFileUrls }
    in
    ( Model
        { time = 0.0
        , window = ( 0, 0 )
        , input = Elm3d.Input.init
        , assets = assets
        , fps = FixedSet.init { maxSize = 30 }
        }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform Viewport
        , Cmd.map Asset assetCmd
        ]
    )



-- UPDATE


type Msg
    = Frame Float
    | Viewport Browser.Dom.Viewport
    | Resize Int Int
    | Input Elm3d.Input.RawEvent
    | Asset Elm3d.Asset.Msg
    | Focus Browser.Events.Visibility
    | ContextMenu


update :
    { toMsg : Msg -> msg
    , toModel : Model -> model
    , model : Model
    , msg : Msg
    , camera : Camera msg
    , nodes : List (Node msg)

    -- , onFrame : Maybe (Context -> msg)
    -- , onInput : Maybe (Event -> msg)
    }
    -> ( model, Cmd msg )
update ({ camera, nodes, msg, toModel, toMsg } as props) =
    let
        (Model model) =
            props.model
    in
    case msg of
        ContextMenu ->
            ( Model model |> toModel
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
                |> toModel
            , Cmd.map Asset assetsCmd
                |> Cmd.map toMsg
            )

        Frame elapsedMs ->
            let
                dt : Float
                dt =
                    elapsedMs / 1000

                time : Float
                time =
                    model.time + dt

                ctx : Elm3d.Node.Context
                ctx =
                    { dt = dt
                    , time = time
                    , input = model.input
                    }

                cameraMsgs : List msg
                cameraMsgs =
                    Elm3d.Camera.update ctx camera

                nodesMsgs : List msg
                nodesMsgs =
                    List.concatMap (Elm3d.Node.update ctx) nodes
            in
            ( Model
                { model
                    | time = time
                    , fps = FixedSet.insert dt model.fps
                }
                |> toModel
            , (cameraMsgs ++ nodesMsgs)
                |> List.map (Task.succeed >> Task.perform identity)
                |> Cmd.batch
            )

        Viewport { scene } ->
            ( Model
                { model
                    | window =
                        ( Basics.floor scene.width
                        , Basics.floor scene.height
                        )
                }
                |> toModel
            , Cmd.none
            )

        Resize width height ->
            ( Model { model | window = ( width, height ) }
                |> toModel
            , Cmd.none
            )

        Focus _ ->
            ( Model { model | input = Elm3d.Input.releaseAllKeys model.input }
                |> toModel
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
                        nodesMsgs =
                            List.concatMap (Elm3d.Node.onInput event) nodes

                        cameraMsgs =
                            Elm3d.Camera.onInput event camera
                    in
                    ( Model { model | input = input } |> toModel
                    , (cameraMsgs ++ nodesMsgs)
                        |> List.map (Task.succeed >> Task.perform identity)
                        |> Cmd.batch
                    )

                Nothing ->
                    ( Model { model | input = input } |> toModel
                    , Cmd.none
                    )


subscriptions :
    { fpsLimit : Maybe Float
    , nodes : List (Node msg)
    , camera : Camera msg
    }
    -> Model
    -> Sub Msg
subscriptions props model =
    Sub.batch
        [ if hasAnyUpdateNodes props model then
            case props.fpsLimit of
                Just fps ->
                    if fps < 60 then
                        Time.every (1000 / fps) (\_ -> Frame (1000 / fps))

                    else
                        Browser.Events.onAnimationFrameDelta Frame

                Nothing ->
                    Browser.Events.onAnimationFrameDelta Frame

          else
            Sub.none
        , Browser.Events.onResize Resize
        , Elm3d.Input.subscriptions Input
        , Browser.Events.onVisibilityChange Focus
        ]


hasAnyUpdateNodes : { props | nodes : List (Node msg), camera : Camera msg } -> Model -> Bool
hasAnyUpdateNodes { nodes, camera } (Model model) =
    Elm3d.Camera.hasUpdateFunction camera
        || List.any Elm3d.Node.hasUpdateFunction nodes


view :
    { showFps : Bool
    , size : ( Int, Int )
    , toMsg : Msg -> msg
    , background : Color
    , nodes : List (Node msg)
    , camera : Camera msg
    }
    -> Model
    -> Html msg
view ({ size, toMsg } as props) (Model model) =
    let
        ( width, height ) =
            size

        directionalLight : Maybe (Node msg)
        directionalLight =
            props.nodes
                |> List.Extra.find Elm3d.Node.isDirectionalLight

        viewCanvas : Html msg
        viewCanvas =
            props.nodes
                |> List.concatMap
                    (Elm3d.Node.toEntity Elm3d.Matrix4.identity
                        { camera = Elm3d.Camera.toMatrix4 size props.camera
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
                    , Html.Attributes.style "background-color" (Elm3d.Color.toHtmlColor props.background)
                    , Html.Attributes.style "transform-origin" "top left"
                    , preventRightClickMenu
                    ]

        preventRightClickMenu =
            Html.Events.custom "contextmenu"
                (Json.Decode.succeed
                    { message = ContextMenu
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                |> Html.Attributes.map toMsg
    in
    if props.showFps then
        Html.div []
            [ viewCanvas
            , viewFps (Model model)
            ]

    else
        viewCanvas



-- DEBUGGING


viewFps : Model -> Html msg
viewFps (Model model) =
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
