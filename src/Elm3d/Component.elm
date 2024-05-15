module Elm3d.Component exposing
    ( Props
    , Model, init
    , Msg, update, subscriptions
    , view
    , toWindowSize
    )

{-|

@docs Props
@docs Model, init
@docs Msg, update, subscriptions
@docs view

@docs toWindowSize

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


type Model
    = Model
        { time : Float
        , window : ( Int, Int )
        , background : Color
        , camera : Camera
        , nodes : List Node
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        , fps : FixedSet Float
        }


toWindowSize : Model -> ( Int, Int )
toWindowSize (Model model) =
    model.window


type alias Props =
    { background : Color
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }


init : Props -> () -> ( Model, Cmd Msg )
init { background, camera, nodes } _ =
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
        , background = background
        , camera = camera
        , nodes = nodes
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
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

                ctx : Elm3d.Node.Context
                ctx =
                    { dt = dt
                    , time = time
                    , input = model.input
                    }
            in
            ( Model
                { model
                    | time = time
                    , camera = Elm3d.Camera.update ctx model.camera
                    , nodes = List.map (Elm3d.Node.update ctx) model.nodes
                    , fps = FixedSet.insert dt model.fps
                }
            , Cmd.none
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

                ( newNodes, newCamera ) =
                    case maybeEvent of
                        Just event ->
                            ( List.map (Elm3d.Node.onInput event) model.nodes
                            , Elm3d.Camera.onInput event model.camera
                            )

                        Nothing ->
                            ( model.nodes
                            , model.camera
                            )
            in
            ( Model
                { model
                    | input = input
                    , camera = newCamera
                    , nodes = newNodes
                }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
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


hasAnyUpdateNodes : Model -> Bool
hasAnyUpdateNodes (Model model) =
    Elm3d.Camera.hasUpdateFunction model.camera
        || List.any Elm3d.Node.hasUpdateFunction model.nodes


view : ( Int, Int ) -> Model -> Html Msg
view size (Model model) =
    let
        ( width, height ) =
            size

        directionalLight : Maybe Node
        directionalLight =
            model.nodes
                |> List.Extra.find Elm3d.Node.isDirectionalLight

        viewCanvas : Html msg
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
                    , Html.Attributes.style "background-color" (Elm3d.Color.toHtmlColor model.background)
                    , Html.Attributes.style "transform-origin" "top left"
                    ]

        viewFps : Html msg
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
        , viewFps
        ]
