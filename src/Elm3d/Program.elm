module Elm3d.Program exposing
    ( Program, View
    , view
    , sandbox
    , element
    )

{-| This module allows you to quickly create a 3D program. If you would like to embed a 3D scene within an existing Elm app, see the [Elm3d.Component](./Elm3d-Component) module.


# **Creating programs**

@docs Program, View


## **1. Stateless programs**

@docs view


## **2. Interactive programs**

@docs sandbox


## **3. Fully-featured programs**

@docs element

-}

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Component
import Elm3d.Frame exposing (Frame)
import Elm3d.Input.Event exposing (Event)
import Elm3d.Internals.Node as Node exposing (Node)
import Elm3d.Internals.Program exposing (..)
import Elm3d.Viewport exposing (Viewport)
import Html exposing (Html)
import Html.Attributes
import Task


{-| A program for creating 3D games and web applications
-}
type alias Program flags model msg =
    Elm3d.Internals.Program.Program flags model msg


{-| This value is returned by your `view` function.
-}
type alias View msg =
    { viewport : Viewport
    , background : Color
    , camera : Camera msg
    , nodes : List (Node msg)
    }


type alias Props flags model msg =
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> View msg
    }


{-| Create a stateless Elm3D program from a view record.

    main : Program () () ()
    main =
        Elm3d.Program.view
            { viewport = Elm3d.Viewport.fullscreen
            , background = Elm3d.Color.white
            , camera =
                Elm3d.Camera.perspective
                    { fov = 60
                    , range = ( 0.01, 1000 )
                    }
            , nodes =
                [ Node.cube
                    { size = 2
                    , color = Elm3d.Color.blue
                    }
                ]
            }

-}
view : View () -> Program () () ()
view view_ =
    sandbox
        { init = \_ -> ()
        , update = \_ m -> m
        , view = \_ -> view_
        }


{-| Create an Elm3D program that can track state.

    main : Program () Model Msg
    main =
        Elm3d.Program.sandbox
            { init = init
            , update = update
            , view = view
            }

    {-|

        type alias Model =
            ...

        init : Model
        init =
            ...

        type Msg
            = ...

        update : Msg -> Model -> Model
        update msg model =
            ...

        view : Model -> Elm3d.Program.View Msg
        view model =
            ...

    -}

-}
sandbox :
    { init : flags -> model
    , update : msg -> model -> model
    , view : model -> View msg
    }
    -> Program flags model msg
sandbox props =
    element
        { init = \flags -> ( props.init flags, Cmd.none )
        , update = \msg model -> ( props.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = props.view
        }


{-| Create an Elm3D program that can track state, send
side effects, and subscribe to events.

    main : Program Flags Model Msg
    main =
        Elm3d.Program.element
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }

    {-|

        type alias Flags =
            ...

        type alias Model =
            ...

        init : Flags -> ( Model, Cmd Msg )
        init flags =
            ...

        type Msg
            = ...

        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg model =
            ...

        subscriptions : Model -> Sub Msg
        subscriptions model =
            ...

        view : Model -> Elm3d.Program.View Msg
        view model =
            ...

    -}

-}
element :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> View msg
    }
    -> Program flags model msg
element props =
    Browser.element
        { init = init props
        , update = update props
        , subscriptions = subscriptions props
        , view = viewFn props
        }


init : Props flags model msg -> flags -> ( Model model, Cmd (Msg msg) )
init props flags =
    let
        ( userModel, userCmd ) =
            props.init flags

        { nodes } =
            props.view userModel
                |> mapView

        ( elm3dModel, elm3dCmd ) =
            Elm3d.Component.init { nodes = nodes }
    in
    ( Model
        { user = userModel
        , elm3d = elm3dModel
        , window = ( 0, 0 )
        }
    , Cmd.batch
        [ userCmd |> Cmd.map User
        , elm3dCmd |> Cmd.map Elm3d
        , Browser.Dom.getViewport
            |> Task.perform Viewport
        ]
    )


update : Props flags model msg -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update props msg (Model model) =
    case msg of
        Elm3d elm3dMsg ->
            let
                { nodes, camera } =
                    props.view model.user
                        |> mapView
            in
            Elm3d.Component.update
                { nodes = nodes
                , camera = camera
                , toMsg = Elm3d
                , toModel = \elm3dModel -> Model { model | elm3d = elm3dModel }
                , model = model.elm3d
                , msg = elm3dMsg

                -- , onFrame = Nothing
                -- , onInput = Nothing
                }

        User userMsg ->
            let
                userTuple : ( model, Cmd msg )
                userTuple =
                    props.update userMsg model.user
            in
            Tuple.mapBoth
                (\userModel -> Model { model | user = userModel })
                (\userCmd -> Cmd.map User userCmd)
                userTuple

        Resize width height ->
            ( Model { model | window = ( width, height ) }
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


mapView : View msg -> View (Msg msg)
mapView v =
    { viewport = v.viewport
    , background = v.background
    , camera = Elm3d.Camera.map User v.camera
    , nodes = List.map (Node.map User) v.nodes

    -- , onFrame = User << v.onFrame
    -- , onInput = User << v.onInput
    }


subscriptions : Props flags model msg -> Model model -> Sub (Msg msg)
subscriptions props (Model { user, elm3d }) =
    let
        { nodes, camera } =
            props.view user
                |> mapView
    in
    Sub.batch
        [ props.subscriptions user
            |> Sub.map User
        , Elm3d.Component.subscriptions
            { fpsLimit = Nothing
            , nodes = nodes
            , camera = camera
            }
            elm3d
            |> Sub.map Elm3d
        , Browser.Events.onResize Resize
        ]


viewFn : Props flags model msg -> Model model -> Html (Msg msg)
viewFn props (Model { user, elm3d, window }) =
    let
        { viewport, background, camera, nodes } =
            props.view user
                |> mapView

        size : ( Int, Int )
        size =
            Elm3d.Viewport.toSize
                window
                viewport

        viewElm3dCanvas : Html (Msg msg)
        viewElm3dCanvas =
            Elm3d.Component.view
                { showFps = False
                , size = size
                , background = background
                , toMsg = Elm3d
                , nodes = nodes
                , camera = camera
                }
                elm3d
    in
    if Elm3d.Viewport.isFullscreen viewport then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; background: black; }""" ]
            , viewElm3dCanvas
            ]

    else if Elm3d.Viewport.isFullscreenAspect viewport then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; height: 100%; background: black; } .elm-3d { height: 100%; display: flex; align-items: center; justify-content: center; }""" ]
            , viewElm3dCanvas
            ]

    else
        viewElm3dCanvas
