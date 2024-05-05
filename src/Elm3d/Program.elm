module Elm3d.Program exposing (Program, new)

import Browser
import Browser.Dom
import Browser.Events
import Elm3d.Camera
import Elm3d.Node
import Elm3d.Window
import Html exposing (Html)
import Html.Attributes
import Task
import WebGL


type alias Program =
    Platform.Program () Model Msg


type Model
    = Model
        { time : Float
        , window : ( Int, Int )
        , nodes : List Elm3d.Node.Node
        }


type Msg
    = Frame Float
    | Viewport Browser.Dom.Viewport
    | Resize Int Int


type alias Props =
    { window : Elm3d.Window.Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Elm3d.Node.Node
    }


new :
    { window : Elm3d.Window.Window
    , camera : Elm3d.Camera.Camera
    , nodes : List Elm3d.Node.Node
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
    ( Model
        { time = 0.0
        , window = ( 0, 0 )
        , nodes = nodes
        }
    , Browser.Dom.getViewport
        |> Task.perform Viewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Frame dt ->
            let
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


updateNode : Elm3d.Node.Context -> Elm3d.Node.Node -> Elm3d.Node.Node
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
        ]


view : Props -> Model -> Html Msg
view { window, camera } (Model model) =
    let
        size : ( Int, Int )
        size =
            Elm3d.Window.toSize model.window window

        ( width, height ) =
            size

        viewWebGlCanvas : Html Msg
        viewWebGlCanvas =
            model.nodes
                |> List.map
                    (Elm3d.Node.toEntity
                        { camera = Elm3d.Camera.toMatrix4 size camera
                        }
                    )
                |> WebGL.toHtmlWith
                    [ WebGL.alpha True
                    , WebGL.depth 1
                    ]
                    [ Html.Attributes.width width
                    , Html.Attributes.height height
                    ]
    in
    if window == Elm3d.Window.fullscreen then
        Html.div [ Html.Attributes.class "elm-3d" ]
            [ Html.node "style" [] [ Html.text """body { margin: 0; overflow: hidden; }""" ]
            , viewWebGlCanvas
            ]

    else
        viewWebGlCanvas
