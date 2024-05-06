module Elm3d.Program exposing (Program, new)

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Asset
import Elm3d.Camera
import Elm3d.Input
import Elm3d.Matrix4
import Elm3d.Node exposing (Node)
import Elm3d.Window
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Task
import WebGL


type alias Program =
    Platform.Program () Model Msg


type Model
    = Model
        { time : Float
        , window : ( Int, Int )
        , nodes : List Node
        , input : Elm3d.Input.Model
        , assets : Elm3d.Asset.Model
        }


type alias Props =
    { window : Elm3d.Window.Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }


new :
    { window : Elm3d.Window.Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Node
    }
    -> Program
new props =
    Browser.element
        { init = init props
        , update = update
        , subscriptions = subscriptions
        , view = view props
        }


init : Props -> () -> ( Model, Cmd Msg )
init { nodes } _ =
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
        , nodes = nodes
        , input = Elm3d.Input.init
        , assets = assets
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
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

                context : Elm3d.Node.Context
                context =
                    { dt = dt
                    , time = time
                    }
            in
            ( Model
                { model
                    | time = time
                    , nodes = List.map (updateNode context) model.nodes
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

        Input event ->
            let
                ( input, maybeEvent ) =
                    Elm3d.Input.update
                        { event = event }
                        model.input

                _ =
                    Debug.log "input" event
            in
            ( Model { model | input = input }
            , Cmd.none
            )


updateNode : Elm3d.Node.Context -> Node -> Node
updateNode context node =
    case Elm3d.Node.toUpdateFunction node of
        Just fn ->
            fn context node

        Nothing ->
            node


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resize
        , Elm3d.Input.subscriptions Input
        ]


view : Props -> Model -> Html Msg
view { window, camera } (Model model) =
    let
        size : ( Int, Int )
        size =
            Elm3d.Window.toSize model.window window

        ( width, height ) =
            size

        directionalLight : Maybe Node
        directionalLight =
            model.nodes
                |> List.Extra.find Elm3d.Node.isDirectionalLight

        viewWebGlCanvas : Html Msg
        viewWebGlCanvas =
            model.nodes
                |> List.concatMap
                    (Elm3d.Node.toEntity Elm3d.Matrix4.identity
                        { camera = Elm3d.Camera.toMatrix4 size camera
                        , light = directionalLight |> Maybe.map Elm3d.Node.toRotation
                        , assets = model.assets
                        }
                    )
                |> WebGL.toHtmlWith
                    [ WebGL.alpha True
                    , WebGL.antialias
                    , WebGL.depth 1
                    ]
                    [ Html.Attributes.width width
                    , Html.Attributes.height height
                    ]
    in
    if Elm3d.Window.isFullscreen window then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """body { margin: 0; overflow: hidden; }""" ]
            , viewWebGlCanvas
            ]

    else if Elm3d.Window.isFullscreenAspect window then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """html, body { margin: 0; overflow: hidden; height: 100%; } .elm-3d { height: 100%; display: flex; align-items: center; justify-content: center; }""" ]
            , viewWebGlCanvas
            ]

    else
        viewWebGlCanvas
