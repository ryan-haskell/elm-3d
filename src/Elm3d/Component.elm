module Elm3d.Component exposing
    ( Model, init
    , Msg, update, subscriptions
    , view
    )

{-| This module allows you to embed Elm3D anywhere you like in an existing application. If you want your whole page to be an Elm3D program, use [Elm3d.Program](./Elm3d-Program) instead.


# **Using the component**

It's important that `init`, `update`, `subscriptions`, and `view` are all wired up to your Elm application, otherwise things won't work as expected.

**Follow the code snippets below** with each type and function to see how you can add this to an existing Elm program.

For a full working example, I recommend taking a look at [Embedded.elm](https://github.com/ryan-haskell/elm-3d/blob/main/example/src/Embedded.elm) from the GitHub repo.

@docs Model, init
@docs Msg, update, subscriptions
@docs view

-}

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Asset
import Elm3d.Color exposing (Color)
import Elm3d.Context exposing (Context)
import Elm3d.Input
import Elm3d.Input.Event exposing (Event)
import Elm3d.Internals.Camera as Camera exposing (Camera)
import Elm3d.Internals.Node as Node exposing (Node)
import Elm3d.Matrix4
import FixedSet exposing (FixedSet)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Task
import Time
import WebGL


{-| Tracks the state of the Elm3D component.

    -- 1️⃣ Add this to your existing `Model`

    import Elm3d.Component

    type alias Model =
        { ...
        , elm3d : Elm3d.Component.Model
        }

-}
type Model
    = Model
        { time : Float
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        , fps : FixedSet Float
        }


{-| -}
type alias View msg =
    { background : Color
    , camera : Camera msg
    , nodes : List (Node msg)
    }


{-| Initialize the Elm3D component, and fire off any initial commands.

    -- 2️⃣ Add this in your `init` function

    import Elm3d.Component
    import Elm3d.Node exposing (Node)


    init : Flags -> ( Model, Cmd Msg )
    init flags =
        let
            (elm3d, elm3dCmd) =
                Elm3d.Component.init
                    { nodes = nodes
                    }
        in
        ( { ...
          , elm3d = elm3d
          }
        , Cmd.map Elm3d elm3dCmd
        )


    nodes : List (Node Msg)
    nodes =
        ...

-}
init :
    { nodes : List (Node msg)
    }
    -> ( Model, Cmd Msg )
init { nodes } =
    let
        objFileUrls : List String
        objFileUrls =
            List.concatMap Node.toObjFileUrls nodes

        ( assets, assetCmd ) =
            Elm3d.Asset.init { objFileUrls = objFileUrls }
    in
    ( Model
        { time = 0.0
        , input = Elm3d.Input.init
        , assets = assets
        , fps = FixedSet.init { maxSize = 30 }
        }
    , Cmd.map Asset assetCmd
    )



-- UPDATE


{-| These messages are handled by the update function.

    -- 3️⃣ Add this to your `Msg` type

    import Elm3d.Component

    type Msg
        = ...
        | Elm3d Elm3d.Component.Msg

-}
type Msg
    = Frame Float
    | Input Elm3d.Input.RawEvent
    | Asset Elm3d.Asset.Msg
    | Focus Browser.Events.Visibility
    | ContextMenu


{-| Updates the Elm3d `Model`, based on the the current `Msg` coming in.

    -- 4️⃣ Add this in your `update` function

    import Elm3d.Component
    import Elm3d.Camera exposing (Camera)


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ...

            Elm3d elm3dMsg ->
                Elm3d.Component.update
                    { msg = elm3dMsg
                    , model = model.elm3d
                    , toModel = \elm3d -> { model | elm3d = elm3d }
                    , toMsg = Elm3d
                    , camera = camera
                    , nodes = nodes
                    }


    camera : Camera msg
    camera =
        ...

-}
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

                ctx : Context
                ctx =
                    { dt = dt
                    , time = time
                    , input = model.input
                    }

                cameraMsgs : List msg
                cameraMsgs =
                    Camera.update ctx camera

                nodesMsgs : List msg
                nodesMsgs =
                    List.concatMap (Node.update ctx) nodes
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
                            List.concatMap (Node.onInput event) nodes

                        cameraMsgs =
                            Camera.onInput event camera
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


{-| This needs to be wired up for input events and any on frame listeners.

    -- 5️⃣ Add this in your `subscriptions` function

    import Elm3d.Component
    import Elm3d.Camera exposing (Camera)


    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.batch
            [ ...
            , Elm3d.Component.subscriptions
                { fpsLimit = Nothing
                , camera = camera
                , nodes = nodes
                }
            ]

-}
subscriptions :
    { fpsLimit : Maybe Float
    , camera : Camera msg
    , nodes : List (Node msg)
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
        , Elm3d.Input.subscriptions Input
        , Browser.Events.onVisibilityChange Focus
        ]


hasAnyUpdateNodes : { props | nodes : List (Node msg), camera : Camera msg } -> Model -> Bool
hasAnyUpdateNodes { nodes, camera } (Model model) =
    Camera.hasUpdateFunction camera
        || List.any Node.hasUpdateFunction nodes


{-| This renders the Elm3D view as HTML for the user to see.

    -- 6️⃣ Add this anywhere in your `view` function


    import Elm3d.Color
    import Elm3d.Component
    import Html exposing (..)

    view : Model -> Html Msg
    view model =
        div []
            [ h1 [] [ text "My app!" ]
            , Elm3d.Component.view
                { showFps = False
                , size = ( 400, 300 )
                , toMsg = Elm3d
                , background = Elm3d.Color.transparent
                , camera = camera
                , nodes = nodes
                }
            ]

-}
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
                |> List.Extra.find Node.isDirectionalLight

        viewCanvas : Html msg
        viewCanvas =
            props.nodes
                |> List.concatMap
                    (Node.toEntity Elm3d.Matrix4.identity
                        { camera = Camera.toMatrix4 size props.camera
                        , light = directionalLight |> Maybe.map Node.toRotation
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
