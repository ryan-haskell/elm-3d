module Elm3d.Program exposing
    ( Program, View
    , static, sandbox, element
    )

{-|

@docs Program, View
@docs static, sandbox, element

-}

import Browser
import Elm3d.Camera exposing (Camera)
import Elm3d.Color exposing (Color)
import Elm3d.Component
import Elm3d.Node exposing (Node)
import Elm3d.Viewport exposing (Viewport)
import Html exposing (Html)
import Html.Attributes


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


type Model model
    = Model
        { elm3d : Elm3d.Component.Model
        , user : model
        }


type Msg msg
    = Elm3d Elm3d.Component.Msg
    | User msg


type alias View msg =
    { viewport : Viewport
    , background : Color
    , camera : Camera msg
    , nodes : List (Node msg)
    }


type alias Props flags model msg =
    { onAssetsLoaded : msg
    , init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> View msg
    }


static : View () -> Program () () ()
static props =
    sandbox
        { onAssetsLoaded = ()
        , init = \_ -> ()
        , update = \_ m -> m
        , view = \_ -> props
        }


sandbox :
    { onAssetsLoaded : msg
    , init : flags -> model
    , update : msg -> model -> model
    , view : model -> View msg
    }
    -> Program flags model msg
sandbox props =
    element
        { onAssetsLoaded = props.onAssetsLoaded
        , init = \flags -> ( props.init flags, Cmd.none )
        , update = \msg model -> ( props.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = props.view
        }


element :
    { onAssetsLoaded : msg
    , init : flags -> ( model, Cmd msg )
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
        , view = view props
        }


init : Props flags model msg -> flags -> ( Model model, Cmd (Msg msg) )
init props flags =
    let
        ( userModel, userCmd ) =
            props.init flags

        { nodes } =
            props.view userModel
                |> mapView props

        ( elm3dModel, elm3dCmd ) =
            Elm3d.Component.init { nodes = nodes }
    in
    ( Model
        { user = userModel
        , elm3d = elm3dModel
        }
    , Cmd.batch
        [ userCmd |> Cmd.map User
        , elm3dCmd |> Cmd.map Elm3d
        ]
    )


update : Props flags model msg -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update props msg (Model model) =
    case msg of
        Elm3d elm3dMsg ->
            let
                { nodes, camera } =
                    props.view model.user
                        |> mapView props
            in
            Elm3d.Component.update
                { nodes = nodes
                , camera = camera
                , toMsg = Elm3d
                , toModel = \elm3dModel -> Model { model | elm3d = elm3dModel }
                , model = model.elm3d
                , msg = elm3dMsg
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


mapView : Props flags model msg -> View msg -> View (Msg msg)
mapView props v =
    let
        fns :
            { toMsg : msg -> Msg msg
            , fromMsg : Msg msg -> msg
            }
        fns =
            { toMsg = User
            , fromMsg =
                \m ->
                    case m of
                        User u ->
                            u

                        _ ->
                            -- TODO: Implement this on `init`
                            props.onAssetsLoaded
            }
    in
    { viewport = v.viewport
    , background = v.background
    , camera = Elm3d.Camera.map fns v.camera
    , nodes = List.map (Elm3d.Node.map fns) v.nodes
    }


subscriptions : Props flags model msg -> Model model -> Sub (Msg msg)
subscriptions props (Model { user, elm3d }) =
    let
        { nodes, camera } =
            props.view user
                |> mapView props
    in
    Sub.batch
        [ props.subscriptions user
            |> Sub.map User
        , Elm3d.Component.subscriptions { nodes = nodes, camera = camera } elm3d
            |> Sub.map Elm3d
        ]


view : Props flags model msg -> Model model -> Html (Msg msg)
view props (Model { user, elm3d }) =
    let
        { viewport, background, camera, nodes } =
            props.view user
                |> mapView props

        size : ( Int, Int )
        size =
            Elm3d.Viewport.toSize
                (Elm3d.Component.toWindowSize elm3d)
                viewport

        viewElm3dCanvas : Html (Msg msg)
        viewElm3dCanvas =
            Elm3d.Component.view
                { size = size
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
